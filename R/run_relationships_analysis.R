################################################################################
## Allen Roberts
## Call functions to perform relationships analysis
################################################################################

## Libraries
library(tidyverse)
library(foreach)
library(doParallel)
library(purrr)

## Set up parallels
numCores <- detectCores()
registerDoParallel(numCores)
print(numCores)

## Run information
suite_name <- "relationships_25__full_scale"
campaign_name <- "Baseline-campaign_orig-relationships"
scale_factor <- 0.05
base_year <- 1960.5

## Cohort settings
min_cohort_year <- 2022
max_cohort_year <- 2031
min_cohort_age <- 15
max_cohort_age <- 35

## File list
files <- list.files(file.path("output", suite_name, campaign_name, "RelationshipEnd"))
tpis <- map(str_split(files, pattern = "_"), ~.x[2]) %>% unlist()

## Function to combine results across different runs
comb <- function(x, ...) {  
  mapply(rbind,x,...,SIMPLIFY=FALSE)
}


## Loop over paramter sets
outputs <- foreach (i=1:length(tpis), .combine = 'comb', .multicombine = TRUE) %dopar% {
  
  ## Analyzer functions
  source(file.path("R", "relationships_analyzers.R"))
  
  tpi <- tpis[i]
  
  ## Create cohort
  cohort_list <- create_cohort(
    suite_name = suite_name,
    campaign_name = campaign_name,
    tpi = tpi,
    min_cohort_year = min_cohort_year,
    max_cohort_year = max_cohort_year,
    min_cohort_age = min_cohort_age,
    max_cohort_age = max_cohort_age
  )
  
  ## Transmissions dataset corresponding to incidence cohort
  inc_trans_list <- create_transmissions_dataset(
    suite_name = suite_name,
    campaign_name = campaign_name,
    tpi = tpi,
    cohort = cohort_list$cohort,
    min_cohort_year = min_cohort_year,
    max_cohort_year = max_cohort_year,
    min_cohort_age = min_cohort_age,
    max_cohort_age = max_cohort_age
  )
  
  ## Relationships analysis 
  relationships_output <- analyze_relationships(
    suite_name = suite_name,
    campaign_name = campaign_name,
    tpi = tpi,
    cohort_risk = cohort_list$cohort_risk,
    inc_trans = inc_trans_list$inc_trans,
    min_cohort_year = min_cohort_year,
    max_cohort_year = max_cohort_year,
    min_cohort_age = min_cohort_age,
    max_cohort_age = max_cohort_age
  )
  
  ## List of outputs to save
  outputs <- c(cohort_list[c("cohort_ir", "cohort_risk_ir")],
               inc_trans_list[c("trans_by_gender_drisk", "trans_by_gender_drisk_srisk", "trans_by_gender_drisk_srisk_scaled","trans_by_gender_drisk_rel")],
               relationships_output)
  
  return(outputs)
}

## Save outputs
save(outputs, file = file.path("output", suite_name, "relationships_results.RData"))


