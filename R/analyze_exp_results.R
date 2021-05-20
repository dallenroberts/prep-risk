################################################################################
## Allen Roberts
## Generate summary plots across runs within EMOD experiments
## Relies on "make_results_summary.R" function.
################################################################################

rm(list = ls())

library(tidyverse)
library(foreach)
library(doParallel)

## Load summary function
source("R/make_results_summary.R")

## Set up parallels
numCores <- detectCores()
registerDoParallel(numCores)

## Enter desired file names here. Could potentially move this into a .csv later
suites <- data.frame("name" = list.files("test-output", pattern = "report"))
# scenarios <- read.csv("scenarios.csv")
# suites <- data.frame("name" = paste0("Baseline-campaign_orig-", scenarios$Scenario))

for(i in 1:nrow(suites)) {
  
  file_path <- paste("test-output", suites$name[i], "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".RData", full.names = FALSE)
  
  print(suites$name[i])
  print(length(file_list))
  
  
}

foreach (i=1:nrow(suites)) %dopar% {
  
  file_path <- paste("test-output", suites$name[i], "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".RData", full.names = FALSE)
  
  report_interval <- 1/as.numeric(substr(suites$name[i], start = nchar(suites$name[i]), stop = nchar(suites$name[i])))
  
  if(!(report_interval %in% c(1, 0.5, 1/4))) {
    
    if(length(file_list) == 0) {
      
      ## Generate results summary
      make_results_summary(name = suites$name[i], make_plots = FALSE, report_interval = report_interval)
    }
    
  }
  
  

  
}

