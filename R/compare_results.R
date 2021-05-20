################################################################################
## Allen Roberts
## Compare results across EMOD experiments
################################################################################

rm(list = ls())

library(tidyverse)

max_clients <- c(2, 30)

files <- sapply(max_clients,function(x) {
  paste0("output/Baseline-campaign_orig-base_case_pop30_maxclients", x, "/ReportHIVByAgeAndGender/output.RData")
})

mylist <- lapply(files, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

for(ii in 1:length(mylist)) {
  
  ## https://stackoverflow.com/questions/30516325/converting-a-list-of-data-frames-into-individual-data-frames-in-r
  
  
}