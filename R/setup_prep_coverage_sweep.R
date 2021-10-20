##############################################################################
## Allen Roberts
## Read existing campaign file, modify PrEP coverage, and write new campaign file
## Also write new scenarios.csv with relevant campaign files
################################################################################

rm(list = ls())

library(tidyverse)
library(jsonlite)

## Coverage levels
eligible_list <- list("High", c("High", "Med"), c("High", "Med", "Low"))
cov_list <- seq(0.1, 0.9, by = 0.1)

for(cov in cov_list) {
  for(elig in eligible_list) {
    
  print(cov)
  print(elig)
  
  ## Load campaign file to modify
  input_campaign <- read_json(path = file.path("InputFiles", "Templates", "campaign_prep_high_med_low_norestart_stratifiedcoverage_zero.json"))
  
  ## Loop through event blocks, find PrEP campaign block, and update coverage
  for(ii in 1:length(input_campaign$Events)) {
    
    ## Right now, update relevant PrEP campaign blocks
    if(grepl("PrEP", input_campaign$Events[[ii]]$Event_Name) & 
       grepl(paste(elig, collapse = "|"), input_campaign$Events[[ii]]$Event_Name)) {
      
      print(input_campaign$Events[[ii]]$Event_Name)
      
      ## Set new coverage level
      input_campaign$Events[[ii]]$Event_Coordinator_Config$Time_Value_Map$Values <- c(cov, cov)
    }
    
  }
  
  ## The "prettify" function resets the indentation and carriage return - otherwise, returns the entire .json in a single line.
  output_campaign <- prettify(toJSON(input_campaign, auto_unbox = TRUE))
  
  ## Save campaign files
  write(output_campaign, file.path("InputFiles", "Templates", paste0("campaign_prep_", paste(tolower(elig), collapse = "_"), "_", 100*cov, ".json")))
  
  }
}

## Create corresponding scenarios file - will need to update this 
base_scenario <- data.frame("Scenario" = "base_case",
                        "Campaign" = "campaign_orig.json")


prep_scenarios <- expand.grid("cov" = cov_list,
                              "eligible" = c("high_med_low", "high_med", "high"))
prep_scenarios$Campaign <- paste0("campaign_prep_", prep_scenarios$eligible, "_", 100*prep_scenarios$cov, ".json")
prep_scenarios$Scenario <- paste0("prep_", prep_scenarios$eligible, "_", 100*prep_scenarios$cov)
prep_scenarios <- prep_scenarios[, c("Scenario", "Campaign")]

## Combine together
scenarios <- rbind(base_scenario, prep_scenarios)
scenarios$Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female <- 6
scenarios$Report_HIV_Period <- as.numeric(365/6)


if(file.exists("scenarios.csv")) {
  stop("Error: scenarios.csv already exists - move manually and rename to avoid overwriting existing file.")
} else {
  write.csv(scenarios, file = "scenarios.csv", row.names = FALSE)
}

