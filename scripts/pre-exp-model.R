# pre-exp-model.R
library(lubridate)
library(tidyverse)


load(".cache/ehd_spells.rda")

dd0 <- ehd_spells %>% 
  ungroup %>% 
  filter(
    !is.na(start), !is.na(stop), !is.na(event), 
    !is.na(ses), stop > start,
    !is.na(age),
    type != "cons",
    age >= 15, age < 50,
    parity > 0,
    parish != "83062"
  ) %>% 
  mutate(
    decade = as.factor(((year(date) %/% 10) * 10)),
    decade = relevel(decade, "1890"),
    ses = as.factor(ses),
    parish = ifelse(is.na(parish), 0, parish)
  )


dd1 <- dd0 %>% 
  replace_na(list(m_nykt_ind = 0, p_nykt_ind = 0, fack_ind = 0)) %>% 
  filter( date >= "1881-01-01", date < "1945-01-01") %>% 
  mutate(
    nykt_all = ifelse(nykt_size > 0 | nykt_nei > 0, TRUE, FALSE),
    fackf_all = ifelse(fackf_size > 0 | fackf_nei > 0, TRUE, FALSE),
    p_mig      = ifelse(p_mig == "stayer", FALSE, TRUE),
    m_mig      = ifelse(m_mig == "stayer", FALSE, TRUE)
  ) %>% 
  replace_na(list(fackf_size3 = "0", nykt_size3 = "0")) 

exp_data <- dd1 %>% 
  group_by(mid, pid, parity, nykt_all, fackf_all, event, 
           decade, ses_label, mig_prop, density,age, children, p_mig, m_mig, parish) %>% 
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
    l_density = log(density),
    mig_prop = mig_prop - mean(mig_prop, na.rm = T),
    mig_prop = mig_prop * 10,
    children = log(children),
    ses = ifelse(ses == "Elite", "Middle class", ses) %>% factor(),
    ses = relevel(ses, "Farmers")
  ) %>% 
  filter(!is.na(ses), !is.na(density), !decade %in% c("1950", "1960")) %>% 
  mutate(parish = factor(parish))


save(exp_data, file = ".cache/exp_data.rda")