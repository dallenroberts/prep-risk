################################################################################
## Allen Roberts
## Look at partnership prevalence by gender, risk group, and age
## Note that this model run is in a strange location
################################################################################


## Libraries
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(viridisLite)
library(RColorBrewer)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot settings
theme_set(theme_classic())

## Set up parallels
numCores <- detectCores()
registerDoParallel(numCores)

## Function to combine results after parallel processing
comb <- function(x, ...) {  
  mapply(rbind,x,...,SIMPLIFY=FALSE)
}

## Run information
scenario_name <- "Baseline-campaign_orig-base_case"
scale_factor <- 0.05
report_interval <- 0.5

## Boolean to save certain results at the run level, rather than just after aggregating across runs
save_runs <- TRUE
## Load EMOD runs
file_path <- paste("Calibrated_RSA_Scenarios", scenario_name, "ReportHIVByAgeAndGender", sep = "/")
file_list <- list.files(file_path, pattern = ".csv")

outputs <- foreach(i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
  
  ## Reload packages - sometimes necessary when parallel computing
  library(tidyverse)
  
  # print(i)
  
  ## Read data 
  df <- read.csv(file.path(file_path, file_list[[i]]), stringsAsFactors = FALSE)
  
  ## Scale up totals based on population scale factor used in the model
  df <- df %>%
    mutate_at(.vars = vars(Population:last_col()), .funs = function(x) x/(scale_factor))
  
  run_num <- run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(file_list[[i]], "_"))[2]))
  df$run_num <- run_num
  
  ## Standardize some variable names
  df <- df %>%
    rename("age_lower" = "Age",
           "gender" = "Gender",
           "risk" = "IP_Key.Risk",
           "year" = "Year",
           "has_transitory" = "Currently..TRANSITORY.",
           "has_informal" = "Currently..INFORMAL.",
           "has_marital" = "Currently..MARITAL.",
           "has_commercial" = "Currently..COMMERCIAL.",
           "has_concurrent" = "Has.Concurrent.Partners",
           "num_partners" = "Current.Partners",
           "num_transitory" = "Num_TRANSITORY",
           "num_informal" = "Num_INFORMAL",
           "num_marital" = "Num_MARITAL",
           "num_commercial" = "Num_COMMERCIAL",
           "num_transitory_concordant" = "Num_Concordant_TRANSITORY",
           "num_informal_concordant" = "Num_Concordant_INFORMAL",
           "num_marital_concordant" = "Num_Concordant_MARITAL",
           "num_commercial_concordant" = "Num_Concordant_COMMERCIAL"
           ) %>%
    mutate(gender = ifelse(gender == 0, "Men", "Women"),
           age_upper = case_when(age_lower == 0 ~ 1,
                                 age_lower == 1 ~ 5,
                                 age_lower == 80 ~ 100,
                                 TRUE ~ age_lower + 5))
  
  ## 
partners <- df %>%
    filter(age_lower >= 15 & age_lower <= 54 & HasHIV == 0) %>%
    group_by(run_num, gender, risk) %>%
    select(run_num, gender, risk, Population, has_marital, has_informal, has_transitory, has_commercial) %>%
  summarise(pct_has_marital = sum(has_marital)/sum(Population),
              pct_has_informal = sum(has_informal/sum(Population)),
              pct_has_transitory = sum(has_transitory)/sum(Population),
              pct_has_commercial = sum(has_commercial)/sum(Population)) %>%
  mutate(age_lower = 15,
         age_upper = 54)
 
partners_age <- df %>%
  filter(age_lower >= 15 & age_lower <= 54 & HasHIV == 0) %>%
  group_by(run_num, gender, risk, age_lower, age_upper) %>%
  select(run_num, gender, risk, age_lower, age_upper, Population, has_marital, has_informal, has_transitory, has_commercial) %>%
  summarise(pct_has_marital = sum(has_marital)/sum(Population),
            pct_has_informal = sum(has_informal/sum(Population)),
            pct_has_transitory = sum(has_transitory)/sum(Population),
            pct_has_commercial = sum(has_commercial)/sum(Population))

  return(list(
    "partners" = partners,
    "partners_age" = partners_age
  ))
  
}

## Calculate summary statistics across runs
for(ii in 1:length(outputs)) {
  
  outputs[[ii]] <- outputs[[ii]] %>%
    pivot_longer(cols = contains("pct_"), names_to = "measure", names_prefix = "pct_has_", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(gender, risk, age_lower, age_upper, measure) %>%
    summarise(mean = mean(value),
              median = median(value),
              lower = quantile(value, probs = 0.025),
              upper = quantile(value, probs = 0.975)) %>%
    mutate(measure = factor(measure, levels = c("marital", "informal", "transitory", "commercial")),
           risk = factor(risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))) 
  
}

## Plots
pdf(file = file.path("figures", "partnership_prevalence.pdf"), height = 6, width = 8)
print(outputs$partners %>%
  ggplot(aes(x = measure, y = median, fill = risk)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
    scale_fill_brewer(palette = "Set1") +
    labs(x = "Partnership Type", y = "Proportion of HIV-negative individuals in partnership") +
    facet_wrap(~gender) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.8)))

print(outputs$partners_age %>%
  ggplot(aes(x = measure, y = median, fill = risk)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Partnership Type", y = "Proportion of HIV-negative individuals in partnership") +
  facet_wrap(~gender + age_lower) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8)))

dev.off()


## Save output
save(outputs, 
     file_path, 
     suite_name, 
     scenario_name, 
     scale_factor,
     file = file.path("scenarios", suite_name, scenario_name, "calibration_results.RData"))

