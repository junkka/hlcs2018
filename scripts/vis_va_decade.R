# vis_va_decade.R
library(coxme)
library(ehahelper)
library(lubridate)
library(tidyverse)

load(".cache/exp_data.rda")
load(".cache/union_model.rda")
load(".cache/temper_model.rda")

# Union -------------

union1 <- exp_data %>% filter(fackf_all == 1) %>% 
  pluck("decade") %>% unique()


tmp_u <- exp_data %>% filter(decade %in% union1) %>% mutate(decade = factor(decade))
new_data_u <- expand.grid(
  fackf_all = unique(tmp_u$fackf_all),
  nykt_all = FALSE,
  ses = unique(tmp_u$ses)[3],
  children = mean(tmp_u$children),
  p_mig = unique(tmp_u$p_mig)[1],
  m_mig = unique(tmp_u$m_mig)[1],
  parish = unique(tmp_u$parish)[1],
  age_2 = mean(tmp_u$age_2),
  mig_prop = mean(tmp_u$mig_prop),
  l_density = mean(tmp_u$l_density),
  parity = 1,
  mid = sample(tmp_u$mid, 1),
  decade = unique(tmp_u$decade)
)

# Predict relative risk
pred_u <- predict_coxme(union_model, new_data_u, type = "lp", se.fit = TRUE)
new_data_u$pred <- as.vector(pred_u$fit)
new_data_u$se.fit <- as.vector(pred_u$se.fit)
# Set confidence intervals
new_data_u <- new_data_u %>% 
  mutate(
    cil = pred - (1.96*se.fit),
    cih = pred + (1.96*se.fit),
    decade = as.character(decade) %>% as.integer(),
    type = "union",
    va = fackf_all
  )

# temperance ---------

temper1 <- exp_data %>% filter(nykt_all == 1) %>% 
  pluck("decade") %>% unique()

tmp_t <- exp_data %>% filter(decade %in% temper1) %>% mutate(decade = factor(decade))
new_data_t <- expand.grid(
  fackf_all = FALSE,
  nykt_all = unique(tmp_t$nykt_all),
  ses = unique(tmp_t$ses)[3],
  children = mean(tmp_t$children),
  p_mig = unique(tmp_t$p_mig)[1],
  m_mig = unique(tmp_t$m_mig)[1],
  parish = unique(tmp_t$parish)[1],
  age_2 = mean(tmp_t$age_2),
  mig_prop = mean(tmp_t$mig_prop),
  l_density = mean(tmp_t$l_density),
  parity = 1,
  mid = sample(tmp_t$mid, 1),
  decade = unique(tmp_t$decade)
)

pred_t <- predict_coxme(temper_model, new_data_t, type = "lp", se.fit = TRUE)

new_data_t$pred <- as.vector(pred_t$fit)
new_data_t$se.fit <- as.vector(pred_t$se.fit)
# Set confidence intervals
new_data_t <- new_data_t %>% 
  mutate(
    cil = pred - (1.96*se.fit),
    cih = pred + (1.96*se.fit),
    decade = as.character(decade) %>% as.integer(),
    type = "temperance",
    va = nykt_all
  )

# Combine dataset and calculate relative risk between member and non-member
dd <- bind_rows(new_data_t, new_data_u) %>% 
  mutate(
    type = factor(type),
    type = fct_recode(type, "Temperance association" = "temperance", "Union"="union"),
    va = factor(va),
    va = fct_recode(va, "Exposed to association"="TRUE","Not exposed"="FALSE"),
    va = fct_rev(va)
  ) %>% 
  mutate(pred = exp(pred), cil = exp(cil), cih = exp(cih)) %>% 
  arrange(type, decade, va) %>%
  group_by(type, decade) %>%
  mutate(
    cih = cih /pred[2],
    cil = cil /pred[2],
    pred = pred /pred[2]
  )

# plot data
p3 <-  ggplot(dd %>% filter(va == "Exposed to association"), aes(decade, pred, group = va)) + 
  geom_hline(aes(yintercept = 1), color = "gray40") +
  geom_line(aes(linetype =va)) +
  geom_point() +
  geom_ribbon(aes(ymin = cil, ymax = cih), alpha = 0.2, color = NA) +
  facet_wrap(~type, nrow = 1) + 
  labs(y = "Relative risk",  x = "Decade", linetype = NULL) + 
  theme_thesis() +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")

h = 3
w = 8
ggsave("figures/va_decade.jpg", p3, height = h, width = w)
ggsave("figures/va_decade.pdf", p3, height = h, width = w)


