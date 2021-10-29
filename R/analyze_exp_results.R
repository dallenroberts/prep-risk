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
suites$id <- 1:nrow(suites)
# suites <- data.frame("name" = list.files("test-output", pattern = "orig"))
# scenarios <- read.csv("scenarios.csv")
# suites <- data.frame("name" = "Baseline-campaign_prep_high_med_low_norestart_stratifiedcoverage-prep_high_med_low_report6")

# for(z in 1:6) {
#   
#   run1 <- 2*(z-1) + 1
#   run2 <- run1 + 1
#   
#   for(i in c(run1, run2)) {
#     
#     file_path <- paste("test-output", suites$name[i], "ReportHIVByAgeAndGender", sep = "/")
#     file_list <- list.files(file_path, pattern = ".RData", full.names = FALSE)
#     
#     print(suites$name[i])
#     print(length(file_list))
#     
#     
#   }
# }  
#  foreach (i=1:nrow(suites)) %dopar% {
    
    
for(i in 1:nrow(suites)) {
  
  print(i)
  file_path <- paste("test-output", suites$name[i], "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".RData", full.names = FALSE)
  
  report_interval <- 1/(2*as.numeric(substr(suites$name[i], start = nchar(suites$name[i]), stop = nchar(suites$name[i]))))
  # report_interval <- 0.5
  
  #   if(length(file_list) == 0) {
  
  ## Generate results summary
  make_results_summary(name = suites$name[i], make_plots = FALSE, report_interval = report_interval)
  
}
    
    # }
    
    # }
#  }
  
  
# }



