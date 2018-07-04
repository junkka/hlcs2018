# post-psm.R
# 
library(broom)
library(stringr)
library(coxme)
library(ehahelper)
library(forcats)
library(tidyverse)


theme_thesis <- function(x){
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey80"),
    legend.background = element_rect(fill = "white", colour = "grey60"),
    legend.key = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = NA)
  )
}

load(".cache/matched_data.rda")


# Matched balance test ------------

match_tab <- matched_data %>% 
  select(va_type, matched_balance) %>% 
  unnest() %>% 
  mutate(label = paste(va_type, sample, sep = "_")) %>% 
  select(-va_type, -sample, -statistic) %>% 
  spread(label, p_val) %>% 
  mutate_if(is.numeric, round, 3)

if (interactive()) knitr::kable(match_tab)

write_csv(match_tab, path = "data/match_tab.csv")

# Multiple-birth model  -------------


n_unique <- function(x) length(unique(x))

coxme_model <- function(x){
  # Random effects model for multiple failures
  x <- filter(x,  year < 1950)
  coxme(Surv(start, stop, event)~va + strata(parity) + (1 | id), data=x)
}

# Run coxme regression function
cox_res <- matched_data %>% 
  mutate(
    cox_model = map(ehd, coxme_model),
    t_m = map(cox_model, tidy, exp = T),
    g_m = map(cox_model, glance, exp = T)
  )

# format regression output
match_desc_fun <- function(X, Y){
  data_frame(term = c("Joiners", "Controls"), estimate = c(X, Y))
}

cox_tab <- cox_res %>% 
  select(va_type, t_m) %>% 
  unnest() %>%
  select(va_type, estimate, p.value, conf.low, conf.high) 

cox_res2 <-cox_res %>% 
  mutate(
    tidy_g = g_m %>% 
      map(~select(., n, events, p, random_n_id, random_sd_id) %>%
            mutate(random_sd_id = exp(random_sd_id)) %>% 
            t() %>% as.data.frame() %>% rownames_to_column %>%
            set_names(c("term", "estimate"))),
    tidy_d = map2(t_m, tidy_g, bind_rows),
    n_con = matched_d %>% map(~n_unique(.$controls)),
    n_treat = matched_d %>% map(~n_unique(.$treated)),
    match_desc = map2(n_treat, n_con, match_desc_fun),
    tidy_d = map2(tidy_d, match_desc, bind_rows)
  ) 
cox_tab <- cox_res2 %>% 
  select(va_type, tidy_d) %>% unnest() %>% 
  select(va_type, term, estimate, p.value) %>% 
  gather(param, val, estimate, p.value) %>% 
  mutate(nme = paste(va_type, param)) %>% 
  select(nme, term, val) %>% 
  spread(nme, val)


cox_tab <-cox_tab[c(8,3,1,4,2,6,7,5), ]

if (interactive()) knitr::kable(cox_tab)

write_csv(cox_tab, path = "data/cox_tab.csv")

# Interaction with year -------------


decade_model <- function(x){
  x <- filter(x, year < 1950) %>% 
    mutate(
      decade = ((year %/% 10)*10) %>% factor(),
      decade = relevel(decade, "1900"))
  coxme(Surv(start, stop, event) ~va*decade + strata(parity) + (1 | id), data = x)
}

# Run model and extract parameters
interaction_model_data <-matched_data %>% 
  mutate(
    seq_model = map(ehd, decade_model),
    t_m = map(seq_model, tidy, exp = T),
    g_m = map(seq_model, glance)
  ) %>% 
  mutate(
    tidy_g = g_m %>% 
      map(~select(., n, events, p, random_n_id, random_sd_id) %>%
            mutate(random_sd_id = exp(random_sd_id)) %>% 
            t() %>% as.data.frame() %>% rownames_to_column %>%
            set_names(c("term", "estimate"))),
    tidy_d = map2(t_m, tidy_g, bind_rows),
    n_con = matched_d %>% map(~n_unique(.$controls)),
    n_treat = matched_d %>% map(~n_unique(.$treated)),
    match_desc = map2(n_treat, n_con, match_desc_fun),
    tidy_d = map2(tidy_d, match_desc, bind_rows)
  ) 

# Tranfrom to tabular format
interaction_tab <- interaction_model_data %>% 
  select(va_type, tidy_d) %>% unnest() %>% 
  select(va_type, term, estimate, p.value) %>% 
  gather(param, val, estimate, p.value) %>% 
  mutate(nme = paste(va_type, param)) %>% 
  select(nme, term, val) %>% 
  spread(nme, val) %>% 
  mutate_if(is.numeric, round, 3)

if (interactive()) knitr::kable(interaction_tab)

write_csv(interaction_tab, path = "data/participation_interaction_tab.csv")

# Interaction visualisation

# set critical value at a 0.95 % conf.int
alpha <- 0.05
crit <- -qnorm(alpha/2)

# Function to extract predicted values
interact_effect <- function(va_type, x, d){
  d <-  filter(d, year < 1950) %>%
    mutate(
      decade = ((year %/% 10)*10) %>% factor(),
      decade = relevel(decade, "1900"))
  # set up new data, for prediction
  newdata <- as_data_frame(expand.grid(
    decade = unique(d$decade),
    va = unique(d$va),
    parity = 1
  ))
  # Run prediction function
  res <- predict_coxme(x, newdata, "lp", TRUE, TRUE)
  # extract fitted values
  newdata$.fitted <- res$fit
  newdata$.se <- res$se.fit
  newdata
}

# Run prediction and extract parameters
interaction_predict <- interaction_model_data %>% 
  mutate(
    est_data = pmap(list(va_type, seq_model, ehd), interact_effect)
  ) %>% 
  select(va_type, est_data) %>% 
  unnest() %>% 
  mutate(
    va = factor(va, labels = c("Non-member", "Member")) %>% fct_rev(),
    conf.low = .fitted - (crit*.se),
    conf.heigh = .fitted + (crit*.se),
    va_type = factor(va_type) %>% fct_recode("Male union" = "fack", "Female temperance" = "m_nykt", "Male temperance" = "p_nykt"),
    .fitted = exp(.fitted),
    conf.low = exp(conf.low),
    conf.heigh = exp(conf.heigh)
  ) 

# Tranform to tabular format
interaction_long <- interaction_predict %>% 
  mutate(rmv = (decade == "1900" & va_type == "Male union")) %>% 
  filter(!rmv) %>% 
  arrange(decade, va_type,va) %>% 
  group_by(va_type, decade) %>% 
  mutate(
    # recalculate relative risk, to non-member controls
    fit = .fitted/.fitted[2],
    conf.low = conf.low/.fitted[2],
    conf.heigh = conf.heigh/.fitted[2]
  )

# Set labels for plot
lbl_d <- data.frame(
  x = c(NA,NA,NA,NA,1923, 1900), 
  y = exp(c(NA,NA,NA,NA,-1, -2)), 
  va = rep(unique(interaction_predict$va), 3),
  label = rep(unique(interaction_predict$va), 3),
  va_type = rep(unique(interaction_predict$va_type), each = 2)
)

# Plot data
decade_plot <- ggplot(filter(interaction_long, va == "Member"), aes(decade, fit, group = va)) + 
  geom_hline(aes(yintercept = 1), color = "gray50") +
  geom_line(aes(linetype = va)) + 
  geom_point() +
  facet_wrap(~va_type) + 
  labs(x = "Decade", y= "Relative risk", linetype = NULL) +
  theme_thesis() +
  theme(legend.position = "none")

if (interactive()) decade_plot

h = 2.5
w = 8
ggsave("figures/decade_plot.png", plot = decade_plot, height = h, width = w)
ggsave("figures/decade_plot.pdf", plot = decade_plot, height = h, width = w)
