# coxme_reg.R
library(coxme)
library(tidyverse)

load(".cache/exp_data.rda")

cox_time <- system.time(
  coxme_exp_model <- coxme(
        Surv(start, stop, event)~
          fackf_all + nykt_all + ses +
          p_mig + m_mig + parish +
          children + age_2 + mig_prop + l_density + decade +
          strata(parity) + (1 | mid),
        data = exp_data
      )

)

cox_time <- (cox_time / 60)/60
coxme_exp_model$runtime <- cox_time

save(coxme_exp_model, file = ".cache/coxme_exp_model.rda")

tmp <- exp_data %>% filter(fackf_all == 1) %>% 
  pluck("decade") %>% unique()

tmp_u <- exp_data %>% filter(decade %in% tmp) %>% mutate(decade = factor(decade))

union_model <- coxme(
  Surv(start, stop, event)~
    fackf_all*decade + nykt_all + ses +
    p_mig + m_mig + parish +
    children + age_2 + mig_prop + l_density +
    strata(parity) + (1 | mid),
  data = tmp_u
)


save(union_model, file = ".cache/union_model.rda")

tmp <- exp_data %>% filter(nykt_all == 1) %>% 
  pluck("decade") %>% unique()
tmp_t <- exp_data %>% filter(decade %in% tmp) %>% mutate(decade = factor(decade))
temper_model <- coxme(
  Surv(start, stop, event)~
    fackf_all + nykt_all*decade + ses +
    p_mig + m_mig + parish +
    children + age_2 + mig_prop + l_density +
    strata(parity) + (1 | mid),
  data = tmp_t
)


save(temper_model, file = ".cache/temper_model.rda")

exp_simple <- coxme(
  Surv(start, stop, event)~
    fackf_all + nykt_all  +
    strata(parity) + (1 | mid),
  data = exp_data
)

save(exp_simple, file = ".cache/exp_simple.rda")

