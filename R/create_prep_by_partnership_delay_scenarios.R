################################################################################
## Allen Roberts
## Create PrEP by partnership, with PrEP delays, scenarios.csv file using KP tags
################################################################################

rm(list = ls())

library(tidyverse)

## Scenario filaname
scenarios_filename <- "scenarios_prep_by_partnership_delay"

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

## RR-1
scenarios_rr_1 <- full_join(
  data.frame(
  "Values__KP_PrEP_Neg_Time_Value_Map" = format_coverage(seq(0.1, 0.8, by = 0.3)),
  "Demographic_Coverage__KP_PrEP_Pos_Coverage" = seq(0.1, 0.8, by = 0.3),
  "Values__KP_PrEP_High_Time_Value_Map" = format_coverage(seq(0.1, 0.8, by = 0.3)),
  "Demographic_Coverage__KP_PrEP_Partner_Coverage" = seq(0.1, 0.8, by = 0.3)
  ),
  data.frame("Values__KP_Delay_Duration_Days" = c(0, 30, 90)),
  by = character()
)

## RR = 2
scenarios_rr_2 <- full_join(
  data.frame(
  "Values__KP_PrEP_Neg_Time_Value_Map" = format_coverage(seq(0.05, 0.45, by = 0.1)),
  "Demographic_Coverage__KP_PrEP_Pos_Coverage" = seq(0.1, 0.9, by = 0.2),
  "Values__KP_PrEP_High_Time_Value_Map" = format_coverage(seq(0.1, 0.9, by = 0.2)),
  "Demographic_Coverage__KP_PrEP_Partner_Coverage" = seq(0.1, 0.9, by = 0.2)
  ),
  data.frame("Values__KP_Delay_Duration_Days" = c(0, 30, 90)),
  by = character()
)

## RR = 3
scenarios_rr_3 <- full_join( 
  data.frame(
  "Values__KP_PrEP_Neg_Time_Value_Map" = format_coverage(seq(0.05, 0.25, by = 0.1)),
  "Demographic_Coverage__KP_PrEP_Pos_Coverage" = seq(0.15, 0.75, by = 0.3),
  "Values__KP_PrEP_High_Time_Value_Map" = format_coverage(seq(0.15, 0.75, by = 0.3)),
  "Demographic_Coverage__KP_PrEP_Partner_Coverage" = seq(0.15, 0.75, by = 0.3)),
  data.frame("Values__KP_Delay_Duration_Days" = c(0, 30, 90)),
  by = character()
)

## RR = 5
scenarios_rr_5 <- full_join(
  data.frame(
  "Values__KP_PrEP_Neg_Time_Value_Map" = format_coverage(seq(0.06, 0.18, by = 0.06)),
  "Demographic_Coverage__KP_PrEP_Pos_Coverage" = seq(0.3, 0.9, by = 0.3),
  "Values__KP_PrEP_High_Time_Value_Map" = format_coverage(seq(0.3, 0.9, by = 0.3)),
  "Demographic_Coverage__KP_PrEP_Partner_Coverage" = seq(0.3, 0.9, by = 0.3)),
  data.frame("Values__KP_Delay_Duration_Days" = c(0, 30, 90)),
  by = character()
)


## Combine together
scenarios <- bind_rows(scenarios_rr_1, scenarios_rr_2, scenarios_rr_3, scenarios_rr_5)

scenarios <- scenarios %>%
  mutate(Campaign = "campaign_prep_by_partnership_delayed.json",
         Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female = 6,
         Report_HIV_Period = 365,
         Report_HIV_ByAgeAndGender_Collect_Intervention_Data = "[\"PrEP_Pos\", \"PrEP_Neg\", \"PrEP_High\", \"PrEP_Partner\"]",
         Scenario = paste(
           "neg",
           extract_coverage(Values__KP_PrEP_Neg_Time_Value_Map),
           "rr",
           100*Demographic_Coverage__KP_PrEP_Pos_Coverage/extract_coverage(Values__KP_PrEP_Neg_Time_Value_Map),
           "delay",
           Values__KP_Delay_Duration_Days,
           sep = "-")
  ) %>%
  relocate(Scenario, Campaign, Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female, Report_HIV_Period)

## Save
if(file.exists("scenarios.csv")) {
  
  stop("Error: scenarios.csv already exists - move manually and rename to avoid overwriting existing file.")
  
} else {
  
  write.csv(scenarios, file = "scenarios.csv", row.names = FALSE)
  write.csv(scenarios, file = file.path("Scenarios", paste0(scenarios_filename, ".csv")), row.names = FALSE)
  
}
