# format_psm_reg.R

library(Matching)
library(tidyverse)
library(survival)
library(broom)
load(".cache/matched_data.rda")


format_numbs <- function(x){
  ifelse(abs(x) > 10, 
         formatC(x, format = "f", digits =0, big.mark=",", drop0trailing = TRUE), 
         formatC(x, format = "f", digits =3, big.mark=",", drop0trailing = FALSE))
}

extract_tbl <- function(x){
  c_t2 <- tidy(x, exp = T) %>%
    select(term, estimate, std.error, p.value)
  
  c_g2 <- glance(x) %>%
    gather(term, estimate)
  
  # c_g2$estimate[c_g2$term == "random_sd_mid"] <- exp(c_g2$estimate[c_g2$term == "random_sd_mid"])
  
  c_t2 %>%
    bind_rows(c_g2[c(1,2,8,13,9), ]) %>%
    mutate_if(is.numeric, format_numbs)
}

update_formu <- function(x){
  paste0(x, " + strata(seq)")
}

run_model <- function(f, x) coxph(formula(f), data= x)

update_data <- function(d, m){
  t_t <- m %>% 
    select(id = treated, seq) %>% 
    distinct() %>% 
    left_join(d, .) %>% 
    mutate(seq = ifelse(event == 0, NA, seq))
  
  t_m <- m %>% 
    select(id = controls, seq2 = seq) %>% 
    distinct() %>% 
    left_join(t_t, .) %>% 
    mutate(seq = ifelse(is.na(seq), seq2, seq)) %>% 
    filter(!is.na(seq))
}

bind_results <- function(x, y) left_join(x, y, by = "term")

t_res <- matched_data %>% 
  mutate(
    post_d = map2(data, matched_d, update_data),
    post_f = map(formula, update_formu),
    post_model = map2(post_f, post_d, run_model),
    res_pre = map(model, extract_tbl),
    res_post = map(post_model, extract_tbl)
  ) 

write_res <- function(va_type, x) {
  nm <- paste0("data/join_res_", va_type, ".csv")
  write_csv(x, nm)
  NULL
}

t_res2 <- t_res %>%
  mutate(
    res = map2(res_pre, res_post, bind_results), 
    w = map2(va_type, res, write_res)
  )



