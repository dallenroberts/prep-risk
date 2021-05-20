################################################################################
## Allen Roberts
## Compare person-time on PrEP with different report intervals
################################################################################

rm(list = ls())

## Libraries
library(ggplot2)
library(tidyverse)
library(binom)
library(viridisLite)
library(foreach)
library(doParallel)
library(purrr)

theme_set(theme_classic())

options(dplyr.summarise.inform=F)
numCores <- detectCores()

# file_list <- list.files("test-output", pattern = "report|base_case")
file_list <- list.files("test-output", pattern = "report")

output <- mclapply(file_list, function(ff) {
  
  load(file.path("test-output", ff, "ReportHIVByAgeAndGender", "output.RData"))
  
  ## Get scenario metadata
  if(ff == "Baseline-campaign_orig-base_case") {
    scenario_name <- "base_case"
    reports_per_year <- 2
  } else {
    scenario_name <- gsub("_norestart.*", "", gsub(".*campaign_", "", ff))
    reports_per_year <- 2*as.integer(gsub(".*_report", "", ff))
  }

  ## Prevalence, ages 15-49, by gender
  prev_adults_gender <- outputs$prev_adults_sex %>%
    group_by(Gender, Year) %>%
    summarise(prev_mean = mean(prev),
              prev_median = median(prev),
              prev_lower = quantile(prev, probs = 0.025),
              prev_upper = quantile(prev, probs = 0.975)) %>%
    mutate(scenario = scenario_name,
           reports_per_year = reports_per_year)
  
  ## Incidence, adults 15-49, by gender
  inc_adults_gender <- outputs$inc_adults_sex %>%
    group_by(Gender, Year) %>%
    summarise(inc_mean = mean(inc),
              inc_median = median(inc),
              inc_lower = quantile(inc, probs = 0.025),
              inc_upper = quantile(inc, probs = 0.975)) %>%
    mutate(scenario = scenario_name,
           reports_per_year = reports_per_year)
  
  ## PrEP coverage, adults by gender and risk
  prep_adults_gender_risk <- outputs$prep_adults_sex_risk %>%
    group_by(Gender, risk_group, Year) %>%
    summarise(prep_mean = mean(prep_cov),
              prep_median = median(prep_cov),
              prep_lower = quantile(prep_cov, probs = 0.025),
              prep_upper = quantile(prep_cov, probs = 0.975)) %>%
    mutate(scenario = scenario_name,
           reports_per_year = reports_per_year)
  
  ## PrEP person-time by age, sex, and risk group
  prep_persontime_age_sex_risk <- outputs$prep_persontime_age_sex_risk_year %>%
    group_by(age_group, Gender, risk_group, run_num) %>%
    summarise(prep_py = sum(prep_py)) %>%
    group_by(age_group, Gender, risk_group) %>%
    summarise(prep_py_mean = mean(prep_py),
              prep_py_median = median(prep_py),
              prep_py_lower = quantile(prep_py, probs = 0.025),
              prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
    mutate(scenario = scenario_name,
           reports_per_year = reports_per_year)
  
  ## Total person-time on PrEP, by risk group
    prep_persontime_risk <- outputs$prep_persontime_age_sex_risk_year %>%
      group_by(risk_group, run_num) %>%
      summarise(prep_py = sum(prep_py)) %>%
      group_by(risk_group) %>%
      summarise(prep_py_mean = mean(prep_py),
                prep_py_median = median(prep_py),
                prep_py_lower = quantile(prep_py, probs = 0.025),
                prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
      mutate(scenario = scenario_name,
             reports_per_year = reports_per_year)
    
    ## Total person-time on PrEP
    prep_persontime_total <- outputs$prep_persontime_age_sex_risk_year %>%
      group_by(run_num) %>%
      summarise(prep_py = sum(prep_py)) %>%
      ungroup() %>%
      summarise(prep_py_mean = mean(prep_py),
                prep_py_median = median(prep_py),
                prep_py_lower = quantile(prep_py, probs = 0.025),
                prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
      mutate(scenario = scenario_name,
             reports_per_year = reports_per_year)
    
    ## Number of new infections, by age, sex, and risk group
    new_infections_age_sex_risk <- outputs$new_infections_age_sex_risk_year %>%
      group_by(age_group, Gender, risk_group, run_num) %>%
      summarise(new_infections = sum(new_infections)) %>%
      group_by(age_group, Gender, risk_group) %>%
      summarise(new_infections_mean = mean(new_infections),
                new_infections_median = median(new_infections),
                new_infections_lower = quantile(new_infections, probs = 0.025),
                new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
      mutate(scenario = scenario_name,
             reports_per_year = reports_per_year)
    
    ## Total new infections, by risk group
    new_infections_risk <- outputs$new_infections_age_sex_risk_year %>%
      group_by(risk_group, run_num) %>%
      summarise(new_infections = sum(new_infections)) %>%
      group_by(risk_group) %>%
      summarise(new_infections_mean = mean(new_infections),
                new_infections_median = median(new_infections),
                new_infections_lower = quantile(new_infections, probs = 0.025),
                new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
      mutate(scenario = scenario_name,
             reports_per_year = reports_per_year)
    
    ## Total new infections
    new_infections_total <- outputs$new_infections_age_sex_risk_year %>%
      group_by(run_num) %>%
      summarise(new_infections = sum(new_infections)) %>%
      ungroup() %>%
      summarise(new_infections_mean = mean(new_infections),
                new_infections_median = median(new_infections),
                new_infections_lower = quantile(new_infections, probs = 0.025),
                new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
      mutate(scenario = scenario_name,
             reports_per_year = reports_per_year)
  
  return(list("prev_adults_gender" = prev_adults_gender,
              "inc_adults_gender" = inc_adults_gender,
              "prep_adults_gender_risk" = prep_adults_gender_risk,
              "prep_persontime_age_sex_risk" = prep_persontime_age_sex_risk,
              "prep_persontime_risk" = prep_persontime_risk,
              "prep_persontime_total" = prep_persontime_total,
              "new_infections_age_sex_risk" = new_infections_age_sex_risk,
              "new_infections_risk" = new_infections_risk,
              "new_infections_total" = new_infections_total))
  
  }, mc.cores = numCores)

## Combine lists - there must be a better way of doing this
## Assumes that all lists have the same number of items, ordered identically
for(dd in 1:length(output[[1]])) {
  
  obj_list <- lapply(1:length(output), function(ii) {
    
    return(output[[ii]][[dd]])
  })
  
  df <- do.call("bind_rows", obj_list)
  
  assign(names(output[[1]][dd]), df)

}

## Save
save(prev_adults_gender,
     inc_adults_gender,
     prep_adults_gender_risk,
     prep_persontime_age_sex_risk,
     prep_persontime_risk,
     prep_persontime_total,
     new_infections_age_sex_risk,
     new_infections_risk,
     new_infections_total,
     file = file.path("output", "report_interval_comparison_prep.RData"))

## Plots
pdf(file = file.path("figures", "report_interval_comparison_prep_plots.pdf"),
    height = 5, width = 7)

## Prevalence, adults 15-49, by gender across scenarios
print(prev_adults_gender %>%
        ggplot(aes(x = Year, y = prev_mean)) +
        geom_line(aes(color = factor(reports_per_year))) +
        geom_ribbon(aes(ymin = prev_lower, ymax = prev_upper, fill = factor(reports_per_year)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(guide = guide_legend(title = "Reports per year")) +
        labs(x = "Year", y = "Prevalence") +
        facet_wrap(~Gender + scenario) +
        ggtitle("Prevalence among adults age 15-49")
)

## Incidence, adults 15-49, by gender across scenarios
print(inc_adults_gender %>%
        ggplot(aes(x = Year, y = inc_mean)) +
        geom_line(aes(color = factor(reports_per_year))) +
        geom_ribbon(aes(ymin = inc_lower, ymax = inc_upper, fill = factor(reports_per_year)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(guide = guide_legend(title = "Reports per year")) +
        labs(x = "Year", y = "Incidence (per 100 PY)") +
        facet_wrap(~Gender + scenario) +
        ggtitle("Incidence among adults age 15-49")
)

## PrEP coverage, adults 15-49, by gender and risk
print(prep_adults_gender_risk %>%
        filter(risk_group == "Low") %>%
        ggplot(aes(x = Year, y = prep_mean)) +
        geom_line(aes(color = factor(reports_per_year))) +
        geom_ribbon(aes(ymin = prep_lower, ymax = prep_upper, fill = factor(reports_per_year)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(guide = guide_legend(title = "Reports per year")) +
        labs(x = "Year", y = "Incidence (per 100 PY)") +
        facet_wrap(~Gender + scenario) +
        ggtitle("PrEP coverage among low risk adults age 15-49")
)

print(prep_adults_gender_risk %>%
        filter(risk_group == "Medium") %>%
        ggplot(aes(x = Year, y = prep_mean)) +
        geom_line(aes(color = factor(reports_per_year))) +
        geom_ribbon(aes(ymin = prep_lower, ymax = prep_upper, fill = factor(reports_per_year)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(guide = guide_legend(title = "Reports per year")) +
        labs(x = "Year", y = "Incidence (per 100 PY)") +
        facet_wrap(~Gender + scenario) +
        ggtitle("PrEP coverage among medium risk adults age 15-49")
)

print(prep_adults_gender_risk %>%
        filter(risk_group == "High") %>%
        ggplot(aes(x = Year, y = prep_mean)) +
        geom_line(aes(color = factor(reports_per_year))) +
        geom_ribbon(aes(ymin = prep_lower, ymax = prep_upper, fill = factor(reports_per_year)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(guide = guide_legend(title = "Reports per year")) +
        labs(x = "Year", y = "Incidence (per 100 PY)") +
        facet_wrap(~Gender + scenario) +
        ggtitle("PrEP coverage among high risk adults age 15-49")
)

## Person-time on PrEP, total
print(prep_persontime_total %>%
  ggplot(aes(x = scenario, y = prep_py_mean, fill = factor(reports_per_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Reports per year")) +
  labs(x = "Scenario", y = "Person-years on PrEP") +
  ggtitle("Total person-time on PrEP, by scenario"))

## Person-time on PrEP, by risk group
print(prep_persontime_risk %>%
  ggplot(aes(x = scenario, y = prep_py_mean, fill = factor(reports_per_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Reports per year")) +
  labs(x = "Scenario", y = "Person-years on PrEP") +
  facet_wrap(~risk_group) +
  ggtitle("Total person-time on PrEP, by scenario"))

## New infections, total
print(new_infections_total %>%
  ggplot(aes(x = scenario, y = new_infections_mean, fill = factor(reports_per_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Reports per year")) +
  labs(x = "Scenario", y = "Number of new infections") +
  ggtitle("Number of new infections, by scenario"))

dev.off()

