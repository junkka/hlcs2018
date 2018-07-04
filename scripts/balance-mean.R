# balance-mean.R

library(tidyverse)
load(".cache/risksets.rda")

sum_factor_distribution <- function(d, var){
  x <- d %>% 
    filter(m_group != "U") %>% 
    group_by(m_group) %>% 
    count_(var) %>% 
    mutate(n = n/sum(n))
  x_t <- d %>% 
    count_(var) %>% 
    mutate(n = n/sum(n), m_group = "A") %>% 
    bind_rows(x)
  
  x_t %>% 
    mutate(n = n*100) %>% 
    spread(m_group, n) %>% 
    mutate(term = var) %>% 
    rename_("variable" = var) %>% 
    select(term, variable, T, C, A)
}

sum_continious_distribution <- function(d, var){
  sumexp <- lazyeval::interp(~mean(x, na.rm = TRUE),
                             x = as.name(var))
  x <- d %>% 
    filter(m_group != "U") %>% 
    group_by(m_group) %>% 
    summarise_(n = sumexp)
  x_t <- d %>%
    summarise_(n = sumexp) %>%
    mutate(m_group = "A") %>%
    bind_rows(x)
  x_t %>%
    spread(m_group, n) %>%
    mutate(term = var, variable = NA) %>% 
    select(term, variable, T, C, A)
}


sum_distributions <- function(d, vars){
  d1 <- distinct(d)
  y <- plyr::ldply(vars, function(x){
    message(x)
    if (class(d1[ ,x]) %in% c("logical", "factor", "character") | length(unique(d1[ ,x])) < 3){
      message("f")
      z <- sum_factor_distribution(d1, x)  
    } else {
      message("c")
      sum_continious_distribution(d1, x)
    }
  })
  
  y
}


run_sum <- function(x){
  vars <- c("parish", "ses_label", "fackf_all", "nykt_all", "ego_mig", 
            "mig_prop","density_g","decade", "ego_age", "children", "p_nykt_ind", "m_nykt_ind")
  sum_distributions(x, vars)
}

ddd <- risksets %>% 
  mutate(
    sum_d = map(riskset, run_sum)
  )

format_n <- function(x, y){
  if (is.na(y)) 
    z <- round(x, 3)
  else
    z <- round(x,1)
  z
}

balanced_mean <- ddd %>% 
  select(va_type, sum_d) %>% 
  unnest() %>% 
  mutate(
    T = format_n(T, variable),
    C = format_n(C, variable),
    A = format_n(A, variable)
  )

write_csv(balanced_mean, path = "data/balanced_mean.csv")
