library(poplinkdata)
library(lubridate)
library(tidyverse)
library(multidplyr)

data(eh_data)

# Add new start date
# from two years before marriage until end
newstart <- eh_data %>%
  filter(type == "marriage") %>% 
  mutate(date = date - years(2), type = "obsstart", offset = 4) %>% 
  select(mid, pid, date, type, offset, marr_date, bdate, m_nykt:fack_ar,sdate, edate)
  
ehd2 <- bind_rows(eh_data, newstart) %>% 
  arrange(mid, date, offset) 

fill_vals <- function(d){
  x <- fill(d, m_nykt_ind:fack_ind, parish, cid, mult_id:con_ind, occu:ses, place, x:population)
  x
}

fill_vals_up <- function(d){
  x <- fill(d, parish, occu:ses, place, x:population, .direction = "up")
  x
}

psm_data <- ehd2 %>%
  arrange(mid, date, offset) %>%
  nest(-mid) %>%
  partition(mid) %>%
  cluster_library("tidyverse") %>%
  cluster_assign_value("fill_vals", fill_vals) %>%
  cluster_assign_value("fill_vals_up", fill_vals_up) %>%
  mutate(data = map(data, fill_vals), data = map(data, fill_vals_up)) %>%
  collect() %>%
  select(mid, data) %>%
  unnest() %>% ungroup()


save(psm_data, file = ".cache/psm_data.rda")
