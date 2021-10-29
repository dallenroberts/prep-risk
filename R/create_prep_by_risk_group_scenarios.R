################################################################################
## Allen Roberts
## Create PrEP by risk group scenarios.csv file using KP tags
################################################################################

rm(list = ls())

library(tidyverse)

## Scenario filaname
scenarios_filename <- "scenarios_prep_by_risk_group"

## Function to transform coverage value into array for time value map
format_coverage <- function(cov) {
  
  paste0("[", cov, ",", cov, "]")
  
}

## Function to extract coverage value from time value map
extract_coverage <- function(time_value_map, pct = TRUE) {
  
  cov <- as.numeric(gsub("\\[", "", str_split(time_value_map, pattern = ",", simplify = TRUE)[, 1]))
    
  ## Convert from proportion to percentage
  if(pct == TRUE) {
    
    cov <- as.integer(100*cov)
    
  }
  
  return(cov)
  
}

## Baseline scearnio - no PrEP
scenarios_baseline <- data.frame(
  "Values__KP_Time_Value_Map_Low_Risk" = format_coverage(0),
  "Values__KP_Time_Value_Map_Medium_Risk" = format_coverage(0),
  "Values__KP_Time_Value_Map_High_Risk" = format_coverage(0)
  
)
## PrEP for all
scenarios_all <- data.frame(
  "Values__KP_Time_Value_Map_Low_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2)),
  "Values__KP_Time_Value_Map_Medium_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2)),
  "Values__KP_Time_Value_Map_High_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2))
)

## PrEP for medium and high risk only
scenarios_med_high <- data.frame(
  "Values__KP_Time_Value_Map_Low_Risk" = format_coverage(0),
  "Values__KP_Time_Value_Map_Medium_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2)),
  "Values__KP_Time_Value_Map_High_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2))
)

## PrEP for high risk only
scenarios_high <- data.frame(
  "Values__KP_Time_Value_Map_Low_Risk" = format_coverage(0),
  "Values__KP_Time_Value_Map_Medium_Risk" = format_coverage(0),
  "Values__KP_Time_Value_Map_High_Risk" = format_coverage(seq(0.2, 0.8, by = 0.2))
)

## Combine together
scenarios <- bind_rows(scenarios_baseline, scenarios_all, scenarios_med_high, scenarios_high)

scenarios <- scenarios %>%
  mutate(Campaign = "campaign_prep_by_risk_group.json",
  Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female = 6,
  Report_HIV_Period = 365,
  Report_HIV_ByAgeAndGender_Collect_Intervention_Data = "[\"PrEP\"]",
  Scenario = paste(extract_coverage(Values__KP_Time_Value_Map_Low_Risk),
                   extract_coverage(Values__KP_Time_Value_Map_Medium_Risk),
                   extract_coverage(Values__KP_Time_Value_Map_High_Risk), sep = "-"),
  Scenario = replace(Scenario, Scenario == "0-0-0", "Baseline")
  ) %>%
  relocate(Scenario, Campaign, Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female, Report_HIV_Period)

## Save
if(file.exists("scenarios.csv")) {
  
  stop("Error: scenarios.csv already exists - move manually and rename to avoid overwriting existing file.")
  
} else {
  
  write.csv(scenarios, file = "scenarios.csv", row.names = FALSE)
  write.csv(scenarios, file = file.path("Scenarios", paste0(scenarios_filename, ".csv")), row.names = FALSE)
  
}
