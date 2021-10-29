################################################################################
## Allen Roberts
## Look at partnership prevalence by gender, risk group, and age
################################################################################

## Libraries
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(viridisLite)
library(RColorBrewer)
library(lemon)

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
suite_name <- "partner-prevalence"
scenario_name <- "Baseline-campaign_orig-partner_prevalence"
scale_factor <- 0.05
report_interval <- 0.5

## Boolean to save certain results at the run level, rather than just after aggregating across runs
save_runs <- TRUE

## Load EMOD runs
file_path <- paste("output", suite_name, scenario_name, "ReportHIVByAgeAndGender", sep = "/")
file_list <- list.files(file_path, pattern = ".csv")

outputs <- foreach(i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
  
  ## Reload packages - sometimes necessary when parallel computing
  library(tidyverse)
  
  # print(i)
  
  ## Read data 
  df <- read.csv(file.path(file_path, file_list[[i]]), stringsAsFactors = FALSE)
  
  ## Filter to years of interest
  df <- df %>%
    filter(Year >= 2022 & Year < 2032)
  
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
    filter(age_lower >= 15 & age_lower <= 34 & HasHIV == 0) %>%
    group_by(run_num, gender, risk) %>%
    select(run_num, gender, risk, Population, has_marital, has_informal, has_transitory, has_commercial, has_concurrent) %>%
  summarise(pct_has_marital = sum(has_marital)/sum(Population),
              pct_has_informal = sum(has_informal/sum(Population)),
              pct_has_transitory = sum(has_transitory)/sum(Population),
              pct_has_commercial = sum(has_commercial)/sum(Population),
              pct_has_concurrent = sum(has_concurrent)/sum(Population)) %>%
  mutate(age_lower = 15,
         age_upper = 34)
 
partners_age <- df %>%
  filter(age_lower >= 15 & age_lower <= 54 & HasHIV == 0) %>%
  group_by(run_num, gender, risk, age_lower, age_upper) %>%
  select(run_num, gender, risk, age_lower, age_upper, Population, has_marital, has_informal, has_transitory, has_commercial, has_concurrent) %>%
  summarise(pct_has_marital = sum(has_marital)/sum(Population),
            pct_has_informal = sum(has_informal/sum(Population)),
            pct_has_transitory = sum(has_transitory)/sum(Population),
            pct_has_commercial = sum(has_commercial)/sum(Population),
            pct_has_concurrent = sum(has_concurrent)/sum(Population))

marital_prev_dhs_2008 <- df %>%
  filter(age_lower >= 20 & age_lower < 50) %>%
  group_by(run_num, gender) %>%
  summarise(pct_has_marital = sum(has_marital)/sum(Population)) %>%
  mutate(age_lower = 20,
         age_upper = 49)

  return(list(
    "partners" = partners,
    "partners_age" = partners_age,
    "marital_prev_dhs_2008" = marital_prev_dhs_2008
  ))
  
}

## Calculate summary statistics across runs
for(ii in 1:length(outputs)) {
  
  outputs[[ii]] <- outputs[[ii]] %>%
    pivot_longer(cols = contains("pct_"), names_to = "measure", names_prefix = "pct_has_", values_to = "value") %>%
    filter(!is.na(value))
  
  if("risk" %in% names(outputs[[ii]])) {
    outputs[[ii]] <- outputs[[ii]] %>%
      group_by(gender, risk, age_lower, age_upper, measure) %>%
      summarise(mean = mean(value),
                median = median(value),
                lower = quantile(value, probs = 0.025),
                upper = quantile(value, probs = 0.975)) %>%
      mutate(measure = factor(measure, levels = c("marital", "informal", "transitory", "commercial", "concurrent"), labels = c("Marital", "Informal", "Transitory", "Commercial", "Concurrent")),
             risk = factor(risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High")))
  } else {
    outputs[[ii]] <- outputs[[ii]] %>%
      group_by(gender, age_lower, age_upper, measure) %>%
      summarise(mean = mean(value),
                median = median(value),
                lower = quantile(value, probs = 0.025),
                upper = quantile(value, probs = 0.975)) %>%
      mutate(measure = factor(measure, levels = c("marital", "informal", "transitory", "commercial", "concurrent"), labels = c("Marital", "Informal", "Transitory", "Commercial", "Concurrent")))
  }
}

## Check marital prevalence against DHS 2008 estimates
## Cited in this study: https://www.tandfonline.com/doi/full/10.1080/17290376.2019.1604254
## Men ages 20-49: 34% married
## Women ages 20-49: 42% married
outputs$marital_prev_dhs_2008

## Our estimates are pretty close to that (34% and 36%, respectively)

## Plots
pdf(file = file.path("figures", "partnership_prevalence.pdf"), height = 6, width = 8)
# print(outputs$partners %>%
#   ggplot(aes(x = measure, y = median, fill = risk)) +
#     geom_bar(stat = "identity", position = position_dodge()) +
#     geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
#     scale_fill_brewer(palette = "Set1") +
#     labs(x = "Partnership Type", y = "Proportion in partnership") +
#     facet_wrap(~gender) +
#     theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) +
#     ggtitle("HIV-negative individuals, 15-34, 2022-2031"))

print(outputs$partners %>%
        filter(measure != "Concurrent") %>%
        ggplot(aes(x = risk, y = median, fill = measure)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
        scale_fill_brewer(palette = "Set1", name = "Relationship type") +
        labs(x = "Risk Group", y = "Proportion in partnership") +
        facet_wrap(~gender) +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) +
        ggtitle("HIV-negative individuals, 15-34, 2022-2031"))

# print(outputs$partners_age %>%
#   ggplot(aes(x = measure, y = median, fill = risk)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
#   scale_fill_brewer(palette = "Set1") +
#   labs(x = "Partnership Type", y = "Proportion of HIV-negative individuals in partnership") +
#   facet_wrap(~gender + age_lower) +
#   theme(axis.text.x = element_text(angle = 30, vjust = 0.8)))

print(outputs$partners_age %>%
        filter(measure != "Concurrent") %>%
        filter(gender == "Men") %>%
        ggplot(aes(x = risk, y = median, fill = measure)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
        scale_fill_brewer(palette = "Set1", name = "Relationship type") +
        labs(x = "Risk Group", y = "Proportion in partnership") +
        facet_rep_wrap(~age_lower, repeat.tick.labels = TRUE) +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) +
        ggtitle("HIV-negative men, 2022-2031"))

print(outputs$partners_age %>%
        filter(measure != "Concurrent") %>%
        filter(gender == "Women") %>%
        ggplot(aes(x = risk, y = median, fill = measure)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
        scale_fill_brewer(palette = "Set1", name = "Relationship type") +
        labs(x = "Risk Group", y = "Proportion in partnership") +
        facet_rep_wrap(~age_lower, repeat.tick.labels = TRUE) +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) +
        ggtitle("HIV-negative women, 2022-2031"))

## Concurrency
print(outputs$partners %>%
        filter(measure == "Concurrent") %>%
        ggplot(aes(x = risk, y = median)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge()) +
        labs(x = "Risk Group", y = "Proportion in concurrent partnership") +
        facet_wrap(~gender) +
        theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) +
        ggtitle("Concurrent partnerships\nHIV-negative individuals, 15-34, 2022-2031"))

dev.off()


## Save output
save(outputs, 
     file_path, 
     suite_name, 
     scenario_name, 
     scale_factor,
     file = file.path("output", suite_name, "partner_prevalence_results.RData"))

