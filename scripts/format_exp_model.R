# format exp_model
library(coxme)
library(ehahelper)
library(broom)
library(forcats)
library(tidyverse)

load(".cache/coxme_exp_model.rda")
load(".cache/exp_simple.rda")

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
    
    c_g2$estimate[c_g2$term == "random_sd_mid"] <- exp(c_g2$estimate[c_g2$term == "random_sd_mid"])
    
    c_t2 %>%
      bind_rows(c_g2[c(1,2,4,5,6,9,10), ]) %>%
      mutate_if(is.numeric, format_numbs)
}

a <- extract_tbl(exp_simple)
b <- extract_tbl(coxme_exp_model) %>% 
  set_names(c("term", "est.b", "std.error.b", "p.value.b"))

d <- full_join(a, b)

write_csv(d, path = "data/coxme_exp.csv")

load(".cache/union_model.rda")
load(".cache/temper_model.rda")


tres <- extract_tbl(temper_model)
ures <- extract_tbl(union_model) %>% 
  set_names(c("term", "est.b", "std.error.b", "p.value.b"))


inter_res <- full_join(tres, ures)

write_csv(inter_res, path="data/inter_exp.csv")
