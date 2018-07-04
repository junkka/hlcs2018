# distribution-matched.R
library(poplinkdata)
library(tidyverse)
library(forcats)

load(".cache/psm_data.rda")

# take the full dataset psm_data link matched and label, 


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


label_va <- function(x){
  x <- factor(x) %>% 
    forcats::fct_recode(
      "Male union" = "fack_ind",
      "Male union" = "fack",
      "Male temperance" = "p_nykt_ind",
      "Male temperance" = "p_nykt",
      "Female temperance" = "m_nykt_ind",
      "Female temperance" = "m_nykt"
    )
  x
}

data("person")

pp <- person %>% select(pid = id, pbdate = bdate)

psm_d2 <- psm_data %>% 
  left_join(pp) %>% 
  replace_na(list(p_nykt_ind = 0, m_nykt_ind = 0, fack_ind = 0, parity = 0)) %>% 
  group_by(mid) %>% 
  mutate(
    sdate = date[type == "obsstart"],
    edate = date[type == "end"],
    topp = max(parity, na.rm = T)
  ) %>% 
  ungroup() 


load(".cache/matched_data.rda")
m_sample <- matched_data %>% select(va_type, matched_d) %>% unnest() %>% tbl_df() %>% 
  gather(state, id, treated, controls) %>%
  distinct() 


m_count <- m_sample %>% 
  distinct(va_type, state, id) %>% 
  count(va_type, state) %>% 
  mutate(va_type = label_va(va_type))

set_id <- function(x, mid, pid){
  ifelse(x == "m_nykt_ind", mid, pid)
}


joiners <- psm_d2 %>%
  distinct(mid, pid, date, offset, p_nykt_ind, m_nykt_ind, fack_ind, topp, bdate, pbdate, sdate, edate, parish) %>% 
  gather(va_type, va, p_nykt_ind, m_nykt_ind, fack_ind) %>% 
  mutate(id = set_id(va_type, mid, pid)) %>% 
  filter(va == 1) %>% 
  mutate(
    va_type = factor(va_type) %>% fct_recode("p_nykt" = "p_nykt_ind", "m_nykt" = "m_nykt_ind", "fack" = "fack_ind")
  ) 

not_linked <- m_sample %>% 
  filter(state == "treated") %>% 
  select(-seq) %>% 
  left_join(joiners) %>% 
  filter(is.na(mid))

joiners <- m_sample %>% 
  filter(state == "treated") %>% 
  left_join(joiners, .) %>% 
  replace_na(list(state = "non"))

joiners <- joiners %>% 
  arrange(mid, date, offset) %>% 
  group_by(id, va_type) %>% 
  slice(1) %>% 
  ungroup()



ms <- joiners %>% 
  mutate(
    diff = poplinkdata:::diff_years(date, sdate),
    diff2 = poplinkdata:::diff_years(date, edate),
    age = poplinkdata:::diff_years(date, pbdate),
    diff = ifelse(state == "treated", seq, diff),
    before = diff < 0,
    after = diff2 > 0,
    timep = ifelse(before, "Before", ifelse(after, "After", "During")),
    timep = ifelse(state == "treated", "During", timep),
    timep = factor(timep, ordered = T, levels = c("Before", "During", "After")),
    va_type = label_va(va_type)
  ) %>% 
  filter(topp > 0 | timep == "During")
  

# select join date
ob_count <- ms %>% count(va_type, timep) %>% rename(state = timep)

label_state <- function(x){
  factor(x, ordered = TRUE, levels = c("Before", "During", "After", "treated", "controls"))
}

a_count <- bind_rows(ob_count, m_count) %>% 
  filter(state != "After") %>% 
  mutate(state = label_state(state)) %>% 
  spread(state, n) %>% 
  mutate(During = treated, Total = Before + During) %>% 
  select(va_type, Total, Before, During, treated, controls)
a_count  

write_csv(a_count, path = "data/matched-count.csv")



clrs <- c("#008FD5", "#FF2700", "#77AB43")

p <- ggplot(filter(ms, timep != "After"), aes(diff, fill = timep)) + 
  geom_histogram(breaks = seq(-27,54, 1), closed = "left") +
  scale_fill_manual(values = c("gray80", clrs[3], clrs[2])) +
  # geom_hline(aes(yintercept = 0), color = "gray80") +
  facet_wrap(~va_type, scales = "free_y") + 
  labs(x = "Years since partnering (two year before marriage)", y = "Frequency", fill = NULL) +
  theme_thesis() +
  theme(legend.position = c(0.9,0.8))

h = 3
w = 10
ggsave("figures/joindist.png", p,height = h, width = w)
ggsave("figures/joindist.pdf", p,height = h, width = w)

