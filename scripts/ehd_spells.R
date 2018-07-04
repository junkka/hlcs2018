# Create episode file (spells) from event history data.
library(poplinkdata)
library(tidyverse)
library(lubridate)

data(eh_data)
data(children)
data(person)

source("R/dates.R")

dd <- eh_data %>% 
  filter(!is.na(pid)) %>% 
  arrange(mid, date, offset) 

# Prepare data as fertility
ehd <- to_fert_data(dd)

# Select one event per simultanious events
ehd2 <- ehd %>% 
  mutate(event = ifelse(type == "cons", 1, 0)) %>% 
  group_by(mid, date) %>% 
  mutate(event = max(event)) %>% 
  filter(row_number() == max(row_number())) %>% 
  ungroup()

ehd_n_haz2 <- ehd2 %>% 
  arrange(mid, date, offset) %>% 
  mutate(nr = row_number())

# Episode end event
end_d <- ehd_n_haz2 %>% 
  mutate(nr = nr - 1) %>% 
  select(mid, nr, enddate = date, endtype = type)

# add end event
com_spell <- ehd_n_haz2 %>% 
  left_join(end_d, by = c("mid", "nr"))

# Add date of previously born child
prev_d <- com_spell %>% 
  filter(type == "cons") %>% 
  select(cid, prev_date = date) %>%
  filter(!is.na(cid)) %>% 
  left_join(com_spell, .)

# Calculate episode time
d1 <- prev_d %>% 
  mutate(
    # From prev
    start = diff_years(date, prev_date), 
    stop = diff_years(enddate, prev_date),
    # From first
    start2 = diff_years(date, (marr_date - years(1))), 
    stop2 = diff_years(enddate, (marr_date - years(1))),
    event = ifelse(endtype == "cons", 1, 0)
  )

# Calculate age
ehd_spells <- d1 %>% 
  mutate(
    age = diff_years(date, bdate),
    age_g = as.factor(((age %/% 5) * 5)),
    age_g = relevel(age_g, "30"),
    ses = ifelse(is.na(ses), "Unknown", ses)
  ) %>% 
  filter(
    !is.na(stop), !is.na(start),
    stop > start
  ) %>% 
  filter(type != "cons")

# Save

save(ehd_spells, file = ".cache/ehd_spells.rda")
