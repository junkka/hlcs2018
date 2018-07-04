library(Matching)
library(survival)
library(poplinkdata)
library(lubridate)
library(broom)
library(tidyverse)

# Load prepared event history dataset
load(".cache/psm_data.rda")

# Load person dataset
# containes static information for each individual in dataset see poplinkdata
data(person)

# Prepare dataset ----------------------------------------------------

# select events between obsstart and end
psm_data2 <- psm_data %>% 
  # replace va membership indicator NA values with 0
  replace_na(list(p_nykt_ind = 0, m_nykt_ind = 0, fack_ind = 0)) %>% 
  # For each relationship, calculate observation start date and end date
  group_by(mid) %>% 
  mutate(
    sdate = date[type == "obsstart"],
    edate = date[type == "end"]
  ) %>% 
  ungroup() %>% 
  # remove events
  filter(date >= sdate, date <= edate)

# Transfrom event data to episode data
psm_data3 <- psm_data2 %>% 
  arrange(mid, date, offset) %>% 
  group_by(mid) %>% 
  mutate(
    stop_date = lead(date)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(stop_date)) %>% 
  mutate(
    start = diff_years(date, sdate),
    stop  = diff_years(stop_date, sdate)
  ) %>% 
  filter(start < stop) %>% 
  group_by(mid) %>% 
  mutate(
    p_event = lead(p_nykt_ind),
    m_event = lead(m_nykt_ind),
    f_event = lead(fack_ind)
  ) %>% ungroup() %>% 
  mutate(
    # Set exposure variabales
    # if a voluntary association exists in area or in neigbours == exposed
    nykt_all  = ifelse(nykt_size > 0 | nykt_nei > 0, TRUE, FALSE),
    fackf_all = ifelse(fackf_size > 0 | fackf_nei > 0, TRUE, FALSE),
    parish = ifelse(parish == 82981, 82980, parish) # combine skelleftea parishes
  )

# Compress episode data, by calculating stop and start dates for each unique 
# combination of variables, thus, removing uneccessary episode breaks.
psm_data4 <- psm_data3 %>% 
  group_by(mid, pid,  marr_date, m_nykt_ind, p_nykt_ind, fack_ind, parish, 
           children, ses_label, place, fackf_all, nykt_all, m_mig, p_mig, 
           mig_prop, density, sdate
  ) %>% 
  summarise(
    start = min(start), 
    stop = max(stop),
    date = min(date),
    stop_date = max(stop_date),
    type1 = first(type),
    type2 = last(type),
    p_event = max(p_event),
    m_event = max(m_event),
    f_event = max(f_event),
    p_nykt_ar = first(p_nykt_ar),
    m_nykt_ar = first(m_nykt_ar),
    fack_ar = first(fack_ar)
  ) %>% 
  ungroup() %>% 
  arrange(mid, date) %>% 
  filter(
    # Select population in sample parishes
    parish %in% c(82980, 82983, 82988, 82990, 83000, 83010)
  ) %>% 
  mutate(
    # Simplify ses variable by combining categories
    ses_label = ifelse(ses_label == "Elite", "Middle class", ses_label),
    ses_label = ifelse(is.na(ses_label), "Unknown", ses_label),
    parish    = as.character(parish),
    # Categorise population density and migration proportion variables
    density_g = cut(density, breaks = quantile(density, na.rm = T), include.lowest = TRUE),
    mig_prop  = cut(mig_prop, breaks = quantile(mig_prop, na.rm = T), include.lowest = TRUE)
  )

# Extract male temperance spells by removing all episodes where 
# the man was a member and by setting the event to p_event
ehd_p <- psm_data4 %>% 
  filter(p_nykt_ind == 0) %>% 
  replace_na(list(p_event = 0)) %>% 
  mutate(va_type = "p_nykt") %>% 
  mutate(ego_mig = p_mig, id = pid, event = p_event)

# Extract female temperance spells
ehd_m <- psm_data4 %>% 
  filter(m_nykt_ind == 0) %>% 
  replace_na(list(m_event = 0)) %>% 
  mutate(va_type = "m_nykt") %>% 
  mutate(ego_mig = m_mig, id = mid, event = m_event)

# Extract male union spells
ehd_f <- psm_data4 %>% 
  filter(fack_ind == 0) %>% 
  replace_na(list(f_event = 0)) %>% 
  mutate(va_type = "fack") %>% 
  mutate(ego_mig = p_mig, id = pid, event = f_event)


# bind into one dataset
a <- bind_rows(ehd_p, ehd_m, ehd_f)

# nest by type of member
# Each operation will be performed on each dataset.
a_n <- a %>% 
  nest(-va_type)

prep_data <- function(x){
  # Data prepreation function.
  # Joiner are only matched to others within the same 
  # period and in the same range of possible parishes and ses classes
  # all observations that do not fit these criterias are filtered out, and all
  # episodes that overlapp with the calendar time window start point are cut at 
  # window start time point.
  y <- x %>% 
    group_by(id) %>% 
    filter(max(event) == 1) %>% 
    ungroup()
  
  y2 <- x %>% 
    filter(event == 1) 
  
  mindate <- min(y2$date)
  x <- x %>% 
    filter(
      ses_label %in% unique(y$ses_label),
      parish %in% unique(y$parish)
    ) %>% 
    mutate(
      is_overlapp = date < mindate & stop_date > mindate,
      date = ifelse(is_overlapp, mindate, date) %>% as.Date(origin = "1970-01-01")
    ) %>% 
    filter(date >= mindate, date <= max(y$stop_date), date < "1940-01-01") %>% 
    mutate(
      start = diff_years(date, sdate),
      stop  = diff_years(stop_date, sdate),
      year = year(date), 
      decade = as.factor((year %/% 10)*10), 
      decade = relevel(decade, "1920"),
      ses_label = factor(ses_label),
      ses_label = relevel(ses_label, "Farmers"),
      ego_mig = ifelse(ego_mig == "internal", "migrant", ego_mig)
    )
  
  # add birth date of ego
  pers <- person %>% distinct(id, bdate)
  
  x <- x %>% 
    left_join(pers) %>% 
    replace_na(list(event = 0, children = 0)) %>% 
    mutate(
      # Calculate ego age
      ego_age = diff_years(date, bdate),
      # Center age around mean age
      ego_age_c = ego_age - mean(ego_age, na.rm = T)
    ) %>% 
    # Keep only observations with non NA values
    filter(!is.na(ego_age), !is.na(mig_prop), !is.na(density_g))
  
  
  x
}

# Run data preparation function
a_n2 <- a_n %>% 
  mutate(
    data = map(data, prep_data)
  )


# Estimate propensity to join ------------------------------------


# Set regression fomulas for each joiner type
formulas <- data_frame(
  va_type = a_n2$va_type,
  formula = c(
    "Surv(start, stop, event) ~ ego_age_c + ses_label + mig_prop +
    parish  + density_g + children + m_nykt_ind +
    fackf_all + nykt_all + ego_mig + decade ",
    "Surv(start, stop, event) ~ ego_age_c + ses_label + mig_prop +
    parish  + density_g + children + p_nykt_ind +
    fackf_all + nykt_all + ego_mig + decade ",
    "Surv(start, stop, event) ~ ego_age_c + ses_label + mig_prop +
    parish  + density_g + children + m_nykt_ind + 
    fackf_all + nykt_all + ego_mig + decade "
  )
)


# Run model that predicts propensity to join
join_model <- function(f, d){
  coxph(formula(f), data= d)
}

a_n3 <- left_join(a_n2, formulas) %>% 
  mutate(
    model = map2(formula, data, join_model)
  )

# Extract regression parameters
a_n4 <- a_n3 %>% 
  mutate(
    tidy_model = map(model, tidy, exp = T)
  ) %>% 
  select(va_type, tidy_model) %>% 
  unnest()

res_a <- a_n4 %>% 
  select(va_type, term, estimate, p.value) %>% 
  gather(type, est, estimate, p.value) %>% 
  mutate(label = paste(va_type, type, sep = "_")) %>% 
  select(-va_type, -type) %>% 
  spread(label, est) %>% 
  mutate_if(is.numeric, round, 3)

# If interactive view regression results
if(interactive()) View(res_a)


# Matching ------------------------------------------------------


match_fun <- function(va_type, d2, fit){
  # Matching function
  
  # Extract all unique joining times
  event_times <- d2$stop[d2$event == 1]  %>% unique() %>% sort()
  
  temp_data <- list()
  
  # run a for loop over the event times
  seq_match <- plyr::llply(event_times, function(x){
    
    # set stop time
    new_data <- d2 %>% 
      # Extract all which were in the riskset at time X 
      filter(stop >= x, start <= x) %>% 
      # set stop time at X 
      mutate(stop = ifelse(event == 0, x, stop)) %>% 
      filter(start < stop, stop == x)
    
    # check that we have observations
    if (nrow(new_data) < 1) {
      message("no newdata")
      return(NULL)
    }
    
    # extract linear predictors for the sample
    new_data$.fitted <- predict(fit, newdata = new_data)
    
    # Extract treated (joiners)
    treated <- new_data %>% filter(event == 1, stop == x)
    
    # Check that we have any observations for joiners
    if(nrow(treated) < 1) {
      message(" fail treated at seq ", x)
      return(NULL)
    }
    
    # Extract non-members
    others <- new_data %>% 
      filter(event != 1)
    # Check that we have any observations for non-members
    if(nrow(others) < 1) {
      message(" fail others")
      return(NULL)
    }
    # Combine back into a riskset
    riskset <- bind_rows(treated, others)
    # Set up variables for matching function
    matched_res <- NA
    # matching variable, the linear predictors
    xs <- riskset$.fitted
    # Exact match
    ex <- FALSE
    # caliper matching value
    caliper_val = NULL
    # Number of matches for each treated
    m = 3
    
    # Run matching function
    tryCatch({
      matched_res <- Match(
        Tr = riskset$event, 
        X = xs,
        replace  = FALSE,
        exact    = ex,
        caliper  = caliper_val,
        estimand = 'ATT',
        M = m
      )    
    }, 
    error = function(mes){
      message("Match error, message:")
      message(mes)
      matched_res <- NA
      return(NULL)
    })
    
    # check that we gave matches
    if (all(is.na(matched_res))) {
      message("no matches")
      return(NULL)
    }
    
    # Extract ids of matched sample
    matched_set <- data_frame(
      treated  = riskset$id[matched_res$index.treated],
      controls = riskset$id[matched_res$index.control],
      seq = x
    )
    
    # Remove matches from the dataset for the next matching sequence
    d2 <<- d2 %>% 
      mutate(
        to_keep = ifelse(
          # if in controls and va or event
          id %in% unique(matched_set$controls) & event == 0,
          FALSE,
          TRUE
        ),
        to_keep = ifelse(
          id %in% unique(matched_set$treated),
          FALSE,
          to_keep
        )
      ) %>% 
      filter(to_keep)
    
    # Extract variables for matched an unmatched sample for later use  
    #  when checking the balance of covariates between groups
    m_riskset <- matched_set %>% 
      transmute(id = treated, treat = 1) %>% 
      left_join(riskset, ., by="id")
    m_riskset <- matched_set %>% 
      transmute(id = controls, contr = 1) %>% 
      distinct() %>% 
      left_join(m_riskset, ., by="id") %>% 
      mutate(
        m_group = ifelse(!is.na(treat), "T", ifelse(!is.na(contr), "C", "U"))
      ) %>% 
      mutate(seq = x)
    
    # Return results for this event time
    list(matched_set = matched_set, riskset = m_riskset)
  }, .progress = "text")
  # Tranfrom results into data.frames and return
  a <- plyr::ldply(seq_match, function(x) x$riskset)
  b <- plyr::ldply(seq_match, function(x) x$matched_set)
  list(risksets = a, matched_sets = b)
}


# Run matching function
a_n5 <- a_n3 %>% 
  mutate(
    matched_d = pmap(list(va_type, data, model), match_fun)
  )


# Extract matching results
a_n6 <- a_n5 %>% 
  mutate(
    riskset = matched_d %>% map("risksets"),
    matched_d = matched_d %>% map("matched_sets")
  )

risksets <- select(a_n6, va_type, riskset)
save(risksets, file = ".cache/risksets.rda")

# Check balance ------------------------------------------------------


balance_hm <- function(vas, var){
  # This function runs the corresponding test comparing the equality of 
  # distributions, depdendent on the type of variable provided (var)
  
  vas_c <- vas %>% filter(m_group == "C") # Matched control group
  vas_t <- vas %>% filter(m_group == "T") # Matched joiners
  vas_b <- vas %>% filter(m_group %in% c("C", "U"), event == 0) # the total non-member population
  
  # Loop over each variable to test
  plyr::ldply(var, function(a){
    message("  ",a)
    x1 <- vas_t[ ,a]
    x2 <- vas_c[ ,a]
    x3 <- vas_b[ ,a]
    
    # check that variable is not univariate
    if (length(unique(x1)) == 1) return(NULL)
    
    if ((length(unique(x1)) < 3) | inherits(x1, c("factor", "character"))){
      # if the variable is categorical or bivariate, run mathelhaen test
      # Compare Joiners to matched controls
      pt1 <- mantelhaen.test(
        x = c(rep("T", length(x1)), rep("C", length(x2))),
        y = c(x1, x2),
        z = c(vas_t$seq, vas_c$seq)
      )
      # Compare Joiners to total non-member population
      pt2 <- mantelhaen.test(
        x = c(rep("T", length(x1)), rep("B", length(x3))),
        y = c(x1, x3),
        z = c(vas_t$seq, vas_b$seq)
      )
      # extract prameters from tests
      p_val <- c(pt1$p.value, pt2$p.value)
      stat  <- c(pt1$statistic, pt2$statistic)
    } else {
      # if the variable is continious run mathelhaen test
      # Compare Joiners to matched controls
      ks1 <-  ks.test(x1, x2)
      # Compare Joiners to total non-member population
      ks2 <- ks.test(x1, x3)
      # Extract parameters from tests
      p_val <- c(ks1$p.value,ks2$p.value)
      stat  <- c(ks1$statistic, ks2$statistic)
    }
    # Combine test results with meta-data into a data.frame
    data_frame(
      variable   = a,
      sample     = c("matched", "unmatched"),
      p_val      = p_val,
      statistic  = as.vector(stat)
    )
    
  })
}


# Set up balance test wrapper function
check_balance <- function(d, fit){
  vas <- as.data.frame(distinct(d))
  # Extract varaible to test from terms in model
  vars <- attr(terms(fit), "term.labels")
  balance_hm(vas, vars)
}

# Run balance test wrapper function
a_n7 <- a_n6 %>%
  mutate(
    matched_balance = map2(riskset, model, check_balance)
  )


# Extract balance test results
bals <- a_n7 %>% select(va_type, matched_balance) 

bals2 <- bals %>% 
  unnest()

tmp <- bals2 %>%
  mutate(label = paste(va_type, sample, sep = "_")) %>% 
  select(-va_type, -sample, -statistic) %>% 
  spread(label, p_val) %>% 
  mutate_if(is.numeric, round, 3)

if(interactive()) View(tmp)

# Extract fertility data for matched sample --------------------------------


# Prepare data for analysis of birth intervals
# A inidvidual is not at risk of having a child during pregnancy
# and eposides between assumed conception date and birth are removed from the 
# dataset, by adding the conception date and birth date of previously born child
# and then removeing all events that fall between these dates.

# Conception dates
cids <- psm_data2 %>% filter(type == "cons") %>% 
  transmute(cid = cid, con_date = date) %>% 
  distinct()
# Birth dates
births <- psm_data2 %>% 
  filter(type == "birth") %>% 
  transmute(cid = cid, birth_date = date) %>% 
  distinct()

# add info to dataset
z <- psm_data2 %>% 
  left_join(cids) %>% 
  left_join(births) %>% 
  filter(!is.na(con_date), !is.na(birth_date))

# Tranfrom event data to episode data and remove pregnancy episodes
z0 <- z %>% 
  arrange(mid, date, offset) %>% 
  group_by(mid) %>% 
  mutate(
    stop_date = lead(date),
    type2 = lead(type)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(stop_date)) %>% 
  mutate(
    # set indivator if episode occures between conception and birth
    not_mid_con = ifelse(date >= con_date & stop_date <= birth_date, F, T) 
  ) %>% 
  filter(not_mid_con) %>% 
  mutate(
    # Set start and stop time for survival analysis, time since previous conception
    start = diff_years(date, con_date),
    stop  = diff_years(stop_date, con_date)
  ) %>% 
  mutate(
    # set event variable and calendar time decade variable
    event = ifelse(type2 == "cons", 1, 0),
    decade = factor((year(date)%/%10)*10),
    decade = relevel(decade, "1920")
  ) %>% 
  group_by(mid) %>% 
  mutate(
    # Recalculate parity from observation start
    partity_old = parity,
    parity = ifelse(lag(partity_old) != partity_old, 1, 0),
    parity = ifelse(is.na(parity), 1, parity),
    parity = ifelse(partity_old == 0, 0, parity), # parity 0 is marriage
    parity = cumsum(parity)
  ) %>% ungroup()


# Extract fertility episode data for matched sample
get_matched_fert <- function(matched, va_type){
  message(va_type)
  # Set up parameters 
  if (va_type == "m_nykt"){
    theid = "mid"
  } else {
    theid = "pid"
  }
  
  # Extract id and matching time (seq) for treated (Joiners)
  trea <- tbl_df(matched) %>% 
    select(id = treated, seq) %>% 
    distinct()
  
  prep_fert_data <- function(x){
    x %>% mutate(
      # New observation start is set to matching time, which is 
      # old observation start time + time of matching (seq)
      # Cut all overlapping spells at new observation start time
      n_start_d = decimal_date(sdate) + seq,
      n_start_d = eha::toDate(n_start_d),
      is_overlapp = ifelse(date < n_start_d & stop_date >n_start_d, TRUE, FALSE),
      date = as.Date(ifelse(is_overlapp == TRUE, n_start_d, date), origin = "1970-01-01")
    ) %>% 
      # remove episodes that occure before new observation time
      filter(date >= n_start_d) %>% 
      mutate(
        # As some episodes have new start times we
        # re-calcualte time to event, from conception date of previous child
        start = diff_years(date, con_date),
        stop  = diff_years(stop_date, con_date)
      ) %>% 
      filter(
        start < stop
      )
  }
  
  # extract fertility episdes for treated sample
  z_t <- z0 %>%  
    rename_("id" = theid) %>% 
    left_join(trea, .) %>% 
    # Set membership as TRUE (1)
    mutate(va = 1) %>% 
    prep_fert_data()
  
  # Extract id and matching time for controls
  contr <- tbl_df(matched) %>% 
    select(id = controls, seq) %>% 
    distinct()
  
  # Run same procedure for controls as for treated
  z_c <- z0 %>% 
    rename_("id" = theid) %>% 
    filter(p_nykt_ind == 0) %>% 
    left_join(contr, .)  %>% 
    # Set membership as FALSE (0)
    mutate(va = 0) %>% 
    prep_fert_data()
    
  
  # combine treated and controls
  z2 <- bind_rows(z_t, z_c)
  
  # Compress episode dataset by variable. 
  z3 <- z2 %>% 
    group_by(id, event, parity, va, seq) %>% 
    summarise(
      date = min(date),
      start = min(start),
      stop = max(stop),
      year = year(min(date))
    ) %>% 
    filter(start < stop) %>% 
    ungroup() %>% 
    group_by(id, parity, va, seq) %>% 
    summarise(
      date = min(date),
      start = min(start),
      stop = max(stop),
      year = year(min(date)),
      event = max(event)
    ) %>% 
    filter(start < stop) %>% 
    ungroup() 
  z3
}

# run function to extract fertility episode data.
matched_data <- a_n7 %>% 
  select(-riskset) %>%
  mutate(
    ehd = pmap(list(matched_d, va_type), get_matched_fert)
  )

# Save in cache for next step in analysis
save(matched_data, file=".cache/matched_data.rda")

