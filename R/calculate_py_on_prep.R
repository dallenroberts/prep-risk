################################################################################
## Allen Roberts
## Estimate person-time on PrEP
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
base_year <- 1960.5

## Cohort settings
min_cohort_year <- 2022
max_cohort_year <- 2031
min_cohort_age <- 15
max_cohort_age <- 35

## File list
campaigns <- list.dirs(file.path("output", suite_name), recursive = FALSE, full.names = FALSE)

## Ignore the baseline campaigns
campaigns <- campaigns[str_count(campaigns, pattern = "Baseline") == 1]

## Loop over campaigns
for(i in 1:length(campaigns)) {
  
  campaign_name <- campaigns[i]
  
  file_path <- file.path("output", suite_name, campaign_name, "ReportEventRecorder")
  file_list <- list.files(file.path(file_path))
  
  ## Loop over paramter sets
  outputs <- foreach (i=1:length(file_list), .combine = 'rbind', .multicombine = TRUE) %dopar% {
    
    ## Libraries
    library(tidyverse)
    
    print(i)
    ff <- file_list[i]
    run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(ff, "_"))[2]))
    
    ## Load Report Event Recorder
    event_recorder <- read.csv(file.path(file_path, ff), stringsAsFactors = FALSE)
    
    event_recorder <- event_recorder %>%
      filter(grepl("Received|Expired", Event_Name) | Event_Name == "PropertyChange" | grepl("Deaths", Event_Name))
    
    event_recorder <- event_recorder %>%
      mutate(age = Age/365,
             gender = ifelse(Gender == "M", "Men", "Women"),
             risk = factor(Risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))) %>%
      rename("year" = "Year",
             "id" = "Individual_ID",
             "infected" = "Infected",
             "event" = "Event_Name")
    
    cohort_prep <- event_recorder %>%
      select(id, gender, age, year, event, risk) %>%
      arrange(id, year) %>%
      group_by(id) %>%
      mutate(PrEP_event = grepl("PrEP", event),
             PrEP_counter = cumsum(PrEP_event)) %>%
      filter(PrEP_counter > 0) %>%
      mutate(event = replace(event, grepl("Death", event), "PrEP_Expired")) %>%
      mutate(py = lead(year) - year,
             on_PrEP = case_when(grepl("Expired", event) ~ 0L,
                                 grepl("Received", event) ~ 1L,
                                 TRUE ~ NA_integer_)) %>%
      fill(on_PrEP, .direction = "down") %>%
      mutate(py = ifelse(is.na(py) & on_PrEP == 1, max_cohort_year + 1 - year, py)) %>%
      filter(!is.na(py))
    
    ## Sum person-time by gender and risk
    cohort_prep_summary <- cohort_prep %>%
      filter(on_PrEP == 1) %>%
      mutate(PrEP_type = ifelse(grepl("Received", event), event, NA)) %>%
      fill(PrEP_type, .direction = "down") %>%
      group_by(gender, risk, PrEP_type) %>%
      summarise(py_on_prep = sum(py)) %>%
      mutate(run_num = run_num)
    
    return(cohort_prep_summary)
    
  }
  
  outputs$campaign <- campaign_name
  
  ## Could think about saving run-specific results here
  if(i == 1) {
    
    py_on_prep_results <- outputs
    
  } else {
    
    py_on_prep_results <- bind_rows(outputs, py_on_prep_results)
  }

}

## Summarise for each campaign
py_on_prep <- py_on_prep_results %>%
  group_by(campaign, gender, run_num) %>%
  summarise(py_on_prep = sum(py_on_prep)) %>%
  group_by(campaign, gender) %>%
  summarise(median = median(py_on_prep),
            mean = mean(py_on_prep),
            lower = quantile(py_on_prep, probs = 0.025),
            upper= quantile(py_on_prep, probs = 0.025)) %>%
  mutate(measure = "py_on_prep")

py_on_prep_by_risk <- py_on_prep_results %>%
  group_by(campaign, gender, risk, run_num) %>%
  summarise(py_on_prep = sum(py_on_prep)) %>%
  group_by(campaign, gender, risk) %>%
  summarise(median = median(py_on_prep),
            mean = mean(py_on_prep),
            lower = quantile(py_on_prep, probs = 0.025),
            upper= quantile(py_on_prep, probs = 0.025)) %>%
  mutate(measure = "py_on_prep")

py_on_prep_by_type <- py_on_prep_results %>%
  group_by(campaign, gender, PrEP_type, run_num) %>%
  summarise(py_on_prep = sum(py_on_prep)) %>%
  group_by(campaign, gender, PrEP_type) %>%
  summarise(median = median(py_on_prep),
            mean = mean(py_on_prep),
            lower = quantile(py_on_prep, probs = 0.025),
            upper= quantile(py_on_prep, probs = 0.025)) %>%
  mutate(measure = "py_on_prep")

py_on_prep_by_type_risk <- py_on_prep_results %>%
  group_by(campaign, gender, risk, PrEP_type) %>%
  summarise(median = median(py_on_prep),
            mean = mean(py_on_prep),
            lower = quantile(py_on_prep, probs = 0.025),
            upper= quantile(py_on_prep, probs = 0.025)) %>%
  mutate(measure = "py_on_prep")

## Save
py_on_prep_results <- list("py_on_prep" = py_on_prep, 
                           "py_on_prep_by_risk" = py_on_prep_by_risk, 
                           "py_on_prep_by_type" = py_on_prep_by_type, 
                           "py_on_prep_by_type_risk" = py_on_prep_by_type_risk)

save(py_on_prep_results, file = file.path("output", suite_name, "py_on_prep.RData"))
