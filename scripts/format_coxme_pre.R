# format_coxme_pre
# Extract parameters and tranform to tidy format
# then write as csv
library(broom)
library(ehahelper)
library(tidyverse)

load(".cache/coxme_pre_match.rda")

c_t <- tidy(coxme_pre_match, exp = TRUE)

c_g <- glance(coxme_pre_match) %>% 
  gather(term, estimate)

c_g$estimate[c_g$term == "random_sd_mid"] <- exp(c_g$estimate[c_g$term == "random_sd_mid"])

c_out <- c_t %>% 
  bind_rows(c_g[c(1,2,4,5,6,9,10), ]) %>% 
  dplyr::select(term, estimate, std.error, p.value) %>% 
  mutate_if(is.numeric, round, 3)

write_csv(c_out, path = "data/coxme_pre_match.csv")
