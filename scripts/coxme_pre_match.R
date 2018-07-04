# coxme_pre_marr.R

load(".cache/ehd_spells.rda")

library(coxme)
library(lubridate)
library(tidyverse)

source("R/dates.R")

ehdata <- ehd_spells %>% 
  replace_na(list(m_nykt_ind = 0, p_nykt_ind = 0, fack_ind = 0))


# Filter va members who joined before partnering
va_data <- function(x){
  d1 <- ehdata %>% 
    rename_("va_year" = x) %>% 
    mutate(
      in_date = as.Date(paste0(va_year, "-01-01")),
      diff = diff_years(marr_date - years(2), in_date),
      diff = ifelse(va_year == 0, NA, diff)
    )
  d1 %>% 
    filter(diff > 0)
}

d1 <- va_data("m_nykt_ar")
d2 <- va_data("p_nykt_ar")
d3 <- va_data("p_fack_ar")

d4 <- ehdata %>% 
  filter(m_nykt_ind == 0, p_nykt_ind == 0, fack_ind == 0)

d5 <- bind_rows(d1, d2, d3,d4)


dd0 <- d5 %>% 
  filter( date >= "1880-01-01", date < "1950-01-01", parish != "83062") %>% 
  mutate(
    nykt_all = ifelse(nykt_size > 0 | nykt_nei > 0, TRUE, FALSE),
    fackf_all = ifelse(fackf_size > 0 | fackf_nei > 0, TRUE, FALSE),
    p_mig      = ifelse(p_mig == "stayer", FALSE, TRUE),
    m_mig      = ifelse(m_mig == "stayer", FALSE, TRUE)
  ) %>% 
  replace_na(list(fackf_size3 = "0", nykt_size3 = "0")) %>% 
  arrange(mid, date, offset) %>% 
  filter(
    !is.na(start), !is.na(stop), !is.na(event), 
    !is.na(ses), stop > start,
    !is.na(age),
    type != "cons",
    age >= 15, age < 50,
    parity > 0
  ) %>% 
  mutate(
    decade = as.factor(((year(date) %/% 10) * 10)),
    decade = relevel(decade, "1890"),
    ses = as.factor(ses),
    parish = ifelse(is.na(parish), 0, parish)
  )


# Compress dataset to used variables
dd1 <- dd0 %>% 
  group_by(mid, pid, parity, p_nykt_ind, m_nykt_ind, fack_ind,
           nykt_all, fackf_all, event, mig_prop, density, parish, ses_label, age,
           children, decade, m_mig,p_mig) %>% 
  summarise(
    start = min(start),
    stop = max(stop),
    date = first(date)
  ) %>% ungroup() %>% 
  mutate(
    ses = ses_label,
    age_2 = age^2,
    parish = as.character(parish),
    decade = factor(decade),
    decade = relevel(decade, "1900"),
    parish = as.factor(parish),
    children = log(children),
    l_density = log(density),
    mig_prop = mig_prop - mean(mig_prop, na.rm = T),
    mig_prop = mig_prop * 10,
    ses = ifelse(ses == "Elite", "Middle class", ses) %>% factor(),
    ses = relevel(ses, "Farmers")
  ) %>% #filter(!is.na(ses), is.na(density)) %>% 
  mutate(parish = factor(parish))

# Run model
cox_time <- system.time(
  coxme_pre_match <- coxme(
    Surv(start, stop, event)~
      fack_ind + p_nykt_ind + m_nykt_ind + fackf_all + nykt_all + ses +
      p_mig + m_mig + parish +
      children + age_2 + mig_prop + l_density + decade + strata(parity) + (1 | mid),
    data = dd1
  )
)

coxme_pre_match$runtime <- cox_time

save(coxme_pre_match, file = ".cache/coxme_pre_match.rda")
