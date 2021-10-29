################################################################################
## Allen Roberts
## Estimate PrEP coverage, number of infections, incidence rate
################################################################################

## Libraries
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)

## Set up parallels
numCores <- detectCores()
registerDoParallel(numCores)

## Function to combine results across different runs
comb <- function(x, ...) {  
  mapply(rbind,x,...,SIMPLIFY=FALSE)
}

## Run information
suite_name <- "prep_partnership_sweep_first25"
scale_factor <- 0.05
report_interval <- 0.5
base_year <- 1960.5

## Cohort settings
min_cohort_year <- 2022
max_cohort_year <- 2031
min_cohort_age <- 15
max_cohort_age <- 35

## File list
campaigns <- list.dirs(file.path("output", suite_name), recursive = FALSE, full.names = FALSE)

## Loop over campaigns
for(i in 1:length(campaigns)) {
  
  campaign_name <- campaigns[i]
  
  file_path <- file.path("output", suite_name, campaign_name, "ReportHIVByAgeAndGender")
  file_list <- list.files(file.path(file_path))
  
  ## Loop over paramter sets
  outputs <- foreach (i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
    
    ## Libraries
    library(tidyverse)
    
    print(i)
    ff <- file_list[i]
    run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(ff, "_"))[2]))
    
    ## Load Report HIV By Age And Gender
    report_hiv <- read.csv(file.path(file_path, ff), stringsAsFactors = FALSE)
    
    ## Scale up totals based on population scale factor used in the model
    report_hiv <- report_hiv %>%
      mutate_at(.vars = vars(Population:Newly.Tested.Negative), .funs = function(x) x/scale_factor)
    
    ## Standardize some variable names
    report_hiv <- report_hiv %>%
      rename("age_lower" = "Age",
             "gender" = "Gender",
             "year" = "Year") %>%
      mutate(gender = ifelse(gender == 0, "Men", "Women"),
             age_upper = case_when(age_lower == 0 ~ 15,
                                   TRUE ~ age_lower + 5),
             risk = case_when(IP_Key.Risk == "HIGH" ~ "High",
                              IP_Key.Risk == "MEDIUM" ~ "Medium",
                              IP_Key.Risk == "LOW" ~ "Low"),
             risk = factor(risk, levels = c("Low", "Medium", "High")),
             run_num = run_num) %>%
      rowwise() %>%
      mutate(on_prep = any(c_across(contains("HasIntervention") & contains("PrEP")) == 1)) %>%
      ungroup()
    
    ## Incidence
    ## Start and end of time interval for incidence calculation
    report_hiv$year_start <- report_hiv$year - report_interval
    report_hiv$year_end <- report_hiv$year ## not needed, but just to make it explicit
    
    ## One-year intervals corresponding to calendar year during which new infections/person-time were accrued
    report_hiv$year_floor <- floor(report_hiv$year_start)
    
    ## Person-time at risk
    report_hiv$person_years_at_risk <- ifelse(report_hiv$HasHIV == 0, report_hiv$Population*report_interval, report_hiv$Newly.Infected*report_interval/2)
    
    ## PrEP coverage among 15-34, grouped by gender and risk
    prep_cov_gender_risk <- report_hiv %>%
      filter(age_lower >= 15 & age_lower < 35 & HasHIV == 0 & year >= min_cohort_year & year <= max_cohort_year) %>%
      group_by(gender, risk, run_num) %>%
      summarise(num_on_prep = sum(Population[on_prep == TRUE]),
                total = sum(Population)) %>%
      mutate(value = num_on_prep/total,
             measure = "prep_coverage",
             age_lower = 15,
             age_upper = 34)
    
    ## PrEP coverage among 15-34, grouped by gender
    prep_cov_gender <- prep_cov_gender_risk %>%
      group_by(gender, run_num) %>%
      summarise(num_on_prep = sum(num_on_prep),
                total = sum(total)) %>%
      mutate(value = num_on_prep/total,
             measure = "prep_coverage",
             age_lower= 15,
             age_upper = 34,
             risk = "All")
    
    ## PrEP coverage among 15-34, grouped by risk
    prep_cov_risk <- prep_cov_gender_risk %>%
      group_by(risk, run_num) %>%
      summarise(num_on_prep = sum(num_on_prep),
                total = sum(total)) %>%
      mutate(value = num_on_prep/total,
             measure = "prep_coverage",
             age_lower = 15,
             age_upper = 34,
             gender = "Both")
    
    ## PrEP coverage among 15-34, overall
    prep_cov <- prep_cov_gender_risk %>%
      group_by(run_num) %>%
      summarise(num_on_prep = sum(num_on_prep),
                total = sum(total)) %>%
      mutate(value = num_on_prep/total,
             measure = "prep_coverage",
             age_lower = 15,
             age_upper = 34,
             gender = "Both",
             risk = "All")
    
    ## Number of infections, entire population, by gender
    num_infections_gender <- report_hiv %>%
      filter(year >= min_cohort_year & year <= max_cohort_year) %>%
      group_by(gender, run_num) %>%
      summarise(value = sum(Newly.Infected)) %>%
      mutate(measure = "num_infections",
             age_lower = 0,
             age_upper = 100,
             risk = "All")
  
    ## Number of infections, entire population
    num_infections <- num_infections_gender %>%
      group_by(run_num) %>%
      summarise(value = sum(value)) %>%
      mutate(measure = "num_infections",
             age_lower = 0,
             age_upper = 100,
             risk = "All",
             gender = "Both")
    
    ## Number of infections, ages 15-34, by gender
    num_infections_15_34_gender <- report_hiv %>%
      filter(age_lower >= 15 & age_lower < 35 & year >= min_cohort_year & year <= max_cohort_year) %>%
      group_by(gender, run_num) %>%
      summarise(value = sum(Newly.Infected)) %>%
      mutate(measure = "num_infections",
             age_lower = 15,
             age_upper = 34,
             risk = "All")
    
    ## Number of infections, ages 15-34
    num_infections_15_34 <- num_infections_15_34_gender %>%
      group_by(run_num) %>%
      summarise(value = sum(value)) %>%
      mutate(measure = "num_infections",
             age_lower = 15,
             age_upper = 34,
             risk = "All",
             gender = "Both")
    
    ## Incidence rate, ages 15-34, by gender
    inc_15_34_gender <- report_hiv %>%
      filter(age_lower >= 15 & age_lower < 35 & year_floor >= min_cohort_year & year_floor <= max_cohort_year) %>%
      group_by(run_num, gender) %>%
      summarise(new_infections = sum(Newly.Infected),
                person_years = sum(person_years_at_risk)) %>%
      mutate(value = 100*new_infections/person_years,
             measure = "incidence",
             age_lower = 15,
             age_upper = 34,
             risk = "All") %>%
      ungroup()
    
    ## Incidence rate, ages 15-34
    inc_15_34 <- report_hiv %>%
      filter(age_lower >= 15 & age_lower < 35 & year_floor >= min_cohort_year & year_floor <= max_cohort_year) %>%
      group_by(run_num) %>%
      summarise(new_infections = sum(Newly.Infected),
                person_years = sum(person_years_at_risk)) %>%
      mutate(value = 100*new_infections/person_years,
             measure = "incidence",
             age_lower = 15,
             age_upper = 34,
             risk = "All",
             gender = "Both") %>%
      ungroup()
    
    ## Return list of outputs
    return(list(
      "prep_cov_gender_risk" = prep_cov_gender_risk,
      "prep_cov_gender" = prep_cov_gender,
      "prep_cov_risk" = prep_cov_risk,
      "prep_cov" = prep_cov,
      "num_infections_gender" = num_infections_gender,
      "num_infections" = num_infections,
      "num_infections_15_34_gender" = num_infections_15_34_gender,
      "num_infections_15_34" = num_infections_15_34,
      "inc_15_34_gender" = inc_15_34_gender,
      "inc_15_34" = inc_15_34
    ))
  }
  
  ## Calculate summary statistics across runs
  for(ii in 1:length(outputs)) {
    
    outputs[[ii]] <- outputs[[ii]] %>%
      group_by(gender, risk, age_lower, age_upper, measure) %>%
      summarise(mean = mean(value),
                median = median(value),
                lower = quantile(value, probs = 0.025),
                upper = quantile(value, probs = 0.975)) %>%
      mutate(campaign = campaign_name)
    
  }
  
  ## Combine lists
  if(i == 1) {
    
    report_hiv_results <- outputs
    
  } else {
    
    report_hiv_results <- comb(report_hiv_results, outputs)
    
  }
  
}

## Save
save(report_hiv_results, file = file.path("output", suite_name, "report_hiv_results.RData"))
