
rm(list = ls())

## Libraries
library(ggplot2)
library(tidyverse)
library(binom)
library(viridisLite)
library(foreach)
library(doParallel)
library(purrr)
library(lemon)

theme_set(theme_classic())

options(dplyr.summarise.inform=F)
numCores <- detectCores()

file_list <- list.files("test-output", pattern = "report6|base_case")

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
    filter(!is.na(prev)) %>%
    summarise(prev_mean = mean(prev),
              prev_median = median(prev),
              prev_lower = quantile(prev, probs = 0.025),
              prev_upper = quantile(prev, probs = 0.975)) %>%
    mutate(scenario = scenario_name)
  
  ## Incidence, adults 15-49, by gender
  inc_adults_gender <- outputs$inc_adults_sex %>%
    group_by(Gender, Year) %>%
    filter(!is.na(inc)) %>%
    summarise(inc_mean = mean(inc),
              inc_median = median(inc),
              inc_lower = quantile(inc, probs = 0.025),
              inc_upper = quantile(inc, probs = 0.975)) %>%
    mutate(scenario = scenario_name)
  
  ## PrEP coverage, adults by gender and risk
  prep_adults_gender_risk <- outputs$prep_adults_sex_risk %>%
    filter(!is.na(prep_cov)) %>%
    group_by(Gender, risk_group, Year) %>%
    summarise(prep_mean = mean(prep_cov),
              prep_median = median(prep_cov),
              prep_lower = quantile(prep_cov, probs = 0.025),
              prep_upper = quantile(prep_cov, probs = 0.975)) %>%
    mutate(scenario = scenario_name)
  
  ## PrEP person-time by age, sex, and risk group
  prep_persontime_age_sex_risk <- outputs$prep_persontime_age_sex_risk_year %>%
    filter(!is.na(prep_py)) %>%
    group_by(age_group, Gender, risk_group, run_num) %>%
    summarise(prep_py = sum(prep_py)) %>%
    mutate(scenario = scenario_name)
  
  ## Total person-time on PrEP, by risk group
  prep_persontime_risk <- outputs$prep_persontime_age_sex_risk_year %>%
    filter(!is.na(prep_py)) %>%
    group_by(risk_group, run_num) %>%
    summarise(prep_py = sum(prep_py)) %>%
    mutate(scenario = scenario_name)
  
  ## Total person-time on PrEP
  prep_persontime_total <- outputs$prep_persontime_age_sex_risk_year %>%
    filter(!is.na(prep_py)) %>%
    group_by(run_num) %>%
    summarise(prep_py = sum(prep_py)) %>%
    mutate(scenario = scenario_name)
  
  ## Number of new infections, by age, sex,risk group, and year
  new_infections_age_sex_risk <- outputs$new_infections_age_sex_risk_year %>%
    filter(!is.na(new_infections)) %>%
    group_by(age_group, Gender, risk_group, Year, run_num) %>%
    summarise(new_infections = sum(new_infections)) %>%
    mutate(scenario = scenario_name)
  
  ## Total new infections, by risk group and year
  new_infections_risk <- outputs$new_infections_age_sex_risk_year %>%
    filter(!is.na(new_infections)) %>%
    group_by(Year, risk_group, run_num) %>%
    summarise(new_infections = sum(new_infections)) %>%
    mutate(scenario = scenario_name)
  
  ## Total new infections by year
  new_infections_year <- outputs$new_infections_age_sex_risk_year %>%
    filter(!is.na(new_infections)) %>%
    group_by(Year, run_num) %>%
    summarise(new_infections = sum(new_infections)) %>%
    mutate(scenario = scenario_name)
  
  ## Total new infections
  new_infections_total <- outputs$new_infections_age_sex_risk_year %>%
    filter(!is.na(new_infections)) %>%
    group_by(run_num) %>%
    summarise(new_infections = sum(new_infections)) %>%
    mutate(scenario = scenario_name)
  
  return(list("prev_adults_gender" = prev_adults_gender,
              "inc_adults_gender" = inc_adults_gender,
              "prep_adults_gender_risk" = prep_adults_gender_risk,
              "prep_persontime_age_sex_risk" = prep_persontime_age_sex_risk,
              "prep_persontime_risk" = prep_persontime_risk,
              "prep_persontime_total" = prep_persontime_total,
              "new_infections_age_sex_risk" = new_infections_age_sex_risk,
              "new_infections_risk" = new_infections_risk,
              "new_infections_year" = new_infections_year,
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

## Results
pdf(file = file.path("figures", "risk_group_results.pdf"), height = 5, width = 7)
## Total number of new infections
print(new_infections_total %>%
  group_by(scenario) %>%
  summarise(new_infections_mean = mean(new_infections),
            new_infections_lower = quantile(new_infections, probs = 0.025),
            new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
  ggplot(aes(x = scenario, y = new_infections_mean)) +
  geom_bar(aes(fill = scenario), stat = "identity") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper)) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = "Scenario", y = "New infections"))

## Total number of infections since 2020
print(new_infections_year %>%
  filter(Year >= 2020) %>%
  group_by(scenario) %>%
  summarise(new_infections_mean = mean(new_infections),
            new_infections_lower = quantile(new_infections, probs = 0.025),
            new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
  ggplot(aes(x = scenario, y = new_infections_mean)) +
  geom_bar(aes(fill = scenario), stat = "identity") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper)) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = "Scenario", y = "New infections since 2020"))

## Total number of infections since 2020, by risk group
print(new_infections_risk %>%
  filter(Year >= 2020) %>%
  group_by(scenario, risk_group) %>%
  summarise(new_infections_mean = mean(new_infections),
            new_infections_lower = quantile(new_infections, probs = 0.025),
            new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = new_infections_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
  labs(x = "Risk group", y = "New infections since 2020"))

## Total number of infections since 2020, by risk group and sex
print(new_infections_age_sex_risk %>%
  filter(Year >= 2020) %>%
  group_by(scenario, Gender, risk_group, run_num) %>%
  summarise(new_infections = sum(new_infections)) %>%
  group_by(scenario, Gender, risk_group) %>%
  summarise(new_infections_mean = mean(new_infections),
            new_infections_lower = quantile(new_infections, probs = 0.025),
            new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = new_infections_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
  labs(x = "Risk group", y = "New infections since 2020") +
  facet_wrap(~Gender))

## Total number of infections since 2020, by age and sex
print(new_infections_age_sex_risk %>%
  filter(Year >= 2020 & age_group %in% c("15-24", "25-34", "35-44", "45-54")) %>%
  group_by(scenario, Gender, age_group, run_num) %>%
  summarise(new_infections = sum(new_infections)) %>%
  group_by(scenario, Gender, age_group) %>%
  summarise(new_infections_mean = mean(new_infections),
            new_infections_lower = quantile(new_infections, probs = 0.025),
            new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
  ggplot(aes(x = Gender, y = new_infections_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = new_infections_lower, ymax = new_infections_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
  labs(x = "Gender", y = "New infections since 2020") +
  facet_wrap(~age_group))

## Infections averted relative to base case
print(new_infections_total %>%
  group_by(run_num) %>%
  mutate(base_case = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case)) %>%
  group_by(scenario) %>%
  summarise(infections_averted_mean = mean(infections_averted),
            infections_averted_lower = quantile(infections_averted, probs = 0.025),
            infections_averted_upper = quantile(infections_averted, probs = 0.975)) %>%
  ggplot(aes(x = scenario, y = infections_averted_mean)) +
  geom_bar(aes(fill = scenario), stat = "identity") + 
  geom_errorbar(aes(ymin = infections_averted_lower, ymax = infections_averted_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Scenario", y = "Infections averted"))

## % infections averted relative to base case
print(new_infections_total %>%
        group_by(run_num) %>%
        mutate(base_case = new_infections[scenario == "base_case"]) %>%
        filter(scenario != "base_case") %>%
        mutate(pct_infections_averted = abs(new_infections - base_case)/base_case) %>%
        group_by(scenario) %>%
        summarise(pct_infections_averted_mean = mean(pct_infections_averted),
                  pct_infections_averted_lower = quantile(pct_infections_averted, probs = 0.025),
                  pct_infections_averted_upper = quantile(pct_infections_averted, probs = 0.975)) %>%
        ggplot(aes(x = scenario, y = 100*pct_infections_averted_mean)) +
        geom_bar(aes(fill = scenario), stat = "identity") + 
        geom_errorbar(aes(ymin = 100*pct_infections_averted_lower, ymax = 100*pct_infections_averted_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
        labs(x = "Scenario", y = "% infections averted"))
  
## Infections averted relative to base case, by risk group
print(new_infections_risk %>%
  group_by(run_num, risk_group, scenario) %>%
  summarise(new_infections = sum(new_infections)) %>%
  group_by(run_num, risk_group) %>%
  mutate(base_case = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case)) %>%
  group_by(scenario, risk_group) %>%
  summarise(infections_averted_mean = mean(infections_averted),
            infections_averted_lower = quantile(infections_averted, probs = 0.025),
            infections_averted_upper = quantile(infections_averted, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = infections_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = infections_averted_lower, ymax = infections_averted_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Risk group", y = "Infections averted"))

## % infections averted relative to base case, by risk group
print(new_infections_risk %>%
        group_by(run_num, risk_group, scenario) %>%
        summarise(new_infections = sum(new_infections)) %>%
        mutate(base_case = new_infections[scenario == "base_case"]) %>%
        filter(scenario != "base_case") %>%
        mutate(pct_infections_averted = abs(new_infections - base_case)/base_case) %>%
        group_by(scenario, risk_group) %>%
        summarise(pct_infections_averted_mean = mean(pct_infections_averted),
                  pct_infections_averted_lower = quantile(pct_infections_averted, probs = 0.025),
                  pct_infections_averted_upper = quantile(pct_infections_averted, probs = 0.975)) %>%
        ggplot(aes(x = risk_group, y = 100*pct_infections_averted_mean, group = scenario)) +
        geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") + 
        geom_errorbar(aes(ymin = 100*pct_infections_averted_lower, ymax = 100*pct_infections_averted_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
        labs(x = "Risk group", y = "% infections averted"))

## Infections averted relative to base case, by sex
print(new_infections_age_sex_risk %>%
        group_by(run_num, Gender, scenario) %>%
        summarise(new_infections = sum(new_infections)) %>%
        group_by(run_num, Gender) %>%
        mutate(base_case = new_infections[scenario == "base_case"]) %>%
        filter(scenario != "base_case") %>%
        mutate(infections_averted = abs(new_infections - base_case)) %>%
        group_by(scenario, Gender) %>%
        summarise(infections_averted_mean = mean(infections_averted),
                  infections_averted_lower = quantile(infections_averted, probs = 0.025),
                  infections_averted_upper = quantile(infections_averted, probs = 0.975)) %>%
        ggplot(aes(x = Gender, y = infections_averted_mean, group = scenario)) +
        geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") + 
        geom_errorbar(aes(ymin = infections_averted_lower, ymax = infections_averted_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
        labs(x = "Gender", y = "Infections averted"))

## % infections averted relative to base case, by sex
print(new_infections_age_sex_risk %>%
        group_by(run_num, Gender, scenario) %>%
        summarise(new_infections = sum(new_infections)) %>%
        mutate(base_case = new_infections[scenario == "base_case"]) %>%
        filter(scenario != "base_case") %>%
        mutate(pct_infections_averted = abs(new_infections - base_case)/base_case) %>%
        group_by(scenario, Gender) %>%
        summarise(pct_infections_averted_mean = mean(pct_infections_averted),
                  pct_infections_averted_lower = quantile(pct_infections_averted, probs = 0.025),
                  pct_infections_averted_upper = quantile(pct_infections_averted, probs = 0.975)) %>%
        ggplot(aes(x = Gender, y = 100*pct_infections_averted_mean, group = scenario)) +
        geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") + 
        geom_errorbar(aes(ymin = 100*pct_infections_averted_lower, ymax = 100*pct_infections_averted_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
        labs(x = "Gender", y = "% infections averted"))


## Infections averated relative to base case, by risk group and sex
print(new_infections_age_sex_risk %>%
  group_by(run_num, Gender, risk_group, scenario) %>%
  summarise(new_infections = sum(new_infections)) %>%
  group_by(run_num, Gender, risk_group) %>%
  mutate(base_case = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case)) %>%
  group_by(scenario, Gender, risk_group) %>%
  summarise(infections_averted_mean = mean(infections_averted),
            infections_averted_lower = quantile(infections_averted, probs = 0.025),
            infections_averted_upper = quantile(infections_averted, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = infections_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = infections_averted_lower, ymax = infections_averted_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Risk group", y = "Infections averted") +
  facet_wrap(~Gender))

## % infections averted relative to base case, by risk group and sex
print(new_infections_age_sex_risk %>%
        group_by(run_num, Gender, risk_group, scenario) %>%
        summarise(new_infections = sum(new_infections)) %>%
        mutate(base_case = new_infections[scenario == "base_case"]) %>%
        filter(scenario != "base_case") %>%
        mutate(pct_infections_averted = abs(new_infections - base_case)/base_case) %>%
        group_by(scenario, Gender, risk_group) %>%
        summarise(pct_infections_averted_mean = mean(pct_infections_averted),
                  pct_infections_averted_lower = quantile(pct_infections_averted, probs = 0.025),
                  pct_infections_averted_upper = quantile(pct_infections_averted, probs = 0.975)) %>%
        ggplot(aes(x = risk_group, y = 100*pct_infections_averted_mean, group = scenario)) +
        geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") + 
        geom_errorbar(aes(ymin = 100*pct_infections_averted_lower, ymax = 100*pct_infections_averted_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
        labs(x = "Risk group", y = "% infections averted") +
        facet_wrap(~Gender))


## Infections averated relative to base case, by risk group and sex and age group
print(new_infections_age_sex_risk %>%
  group_by(run_num, Gender, age_group, risk_group, scenario) %>%
  summarise(new_infections = sum(new_infections)) %>%
  group_by(run_num, Gender, age_group, risk_group) %>%
  mutate(base_case = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case)) %>%
  group_by(scenario, Gender, age_group, risk_group) %>%
  summarise(infections_averted_mean = mean(infections_averted),
            infections_averted_lower = quantile(infections_averted, probs = 0.025),
            infections_averted_upper = quantile(infections_averted, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = infections_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = infections_averted_lower, ymax = infections_averted_upper), position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Risk group", y = "Infections averted") +
  facet_wrap(~Gender + age_group, nrow = 2))

## Person-time on PrEP, total
print(prep_persontime_total %>%
        group_by(scenario) %>%
        summarise(prep_py_mean = mean(prep_py),
                  prep_py_lower = quantile(prep_py, probs = 0.025),
                  prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
        ggplot(aes(x = scenario, y = prep_py_mean, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
        labs(x = "Scenario", y = "Person-years on PrEP"))

## Person-time on PrEP, by risk group
print(prep_persontime_risk %>%
        group_by(scenario, risk_group) %>%
        summarise(prep_py_mean = mean(prep_py),
                  prep_py_lower = quantile(prep_py, probs = 0.025),
                  prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
        ggplot(aes(x = risk_group, y = prep_py_mean, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
        labs(x = "Risk group", y = "Person-years on PrEP"))

## Person-time on PrEP, by risk group and sex
print(prep_persontime_age_sex_risk %>%
        group_by(scenario, Gender, risk_group, run_num) %>%
        summarise(prep_py = sum(prep_py)) %>%
        group_by(scenario, Gender, risk_group) %>%
        summarise(prep_py_mean = mean(prep_py),
                  prep_py_lower = quantile(prep_py, probs = 0.025),
                  prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
        ggplot(aes(x = risk_group, y = prep_py_mean, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
        labs(x = "Risk group", y = "Person-years on PrEP") +
        facet_wrap(~Gender))

## Person-time on PrEP, by age and sex
print(prep_persontime_age_sex_risk %>%
        filter(age_group %in% c("15-24", "25-34", "35-44", "45-54")) %>%
        group_by(scenario, Gender, age_group, run_num) %>%
        summarise(prep_py = sum(prep_py)) %>%
        group_by(scenario, Gender, age_group) %>%
        summarise(prep_py_mean = mean(prep_py),
                  prep_py_lower = quantile(prep_py, probs = 0.025),
                  prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
        ggplot(aes(x = Gender, y = prep_py_mean, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
        labs(x = "Age group", y = "Person-years on PrEP") +
        facet_wrap(~age_group))

## Person-time on PrEP, by age and sex and risk_gorup
print(prep_persontime_age_sex_risk %>%
        filter(age_group %in% c("15-24", "25-34", "35-44", "45-54")) %>%
        group_by(scenario, Gender, age_group, risk_group, run_num) %>%
        summarise(prep_py = sum(prep_py)) %>%
        group_by(scenario, Gender, age_group, risk_group) %>%
        summarise(prep_py_mean = mean(prep_py),
                  prep_py_lower = quantile(prep_py, probs = 0.025),
                  prep_py_upper = quantile(prep_py, probs = 0.975)) %>%
        ggplot(aes(x = risk_group, y = prep_py_mean, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = prep_py_lower, ymax = prep_py_upper), position = position_dodge(0.9)) +
        scale_fill_viridis_d(guide = guide_legend(title = "Scenario")) +
        labs(x = "Risk group", y = "Person-years on PrEP") +
        facet_wrap(~Gender + age_group, nrow = 2))

## Person-time on PrEP per infection averted
print(prep_persontime_total %>%
  left_join(new_infections_total, by = c("run_num", "scenario")) %>%
  group_by(run_num) %>%
  mutate(base_case_infections = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case_infections),
         prep_py_per_infection_averted = prep_py/infections_averted) %>%
  group_by(scenario) %>%
  summarise(prep_py_per_infection_averted_mean = mean(prep_py_per_infection_averted),
            prep_py_per_infection_averted_lower = quantile(prep_py_per_infection_averted, probs = 0.025),
            prep_py_per_infection_averted_upper = quantile(prep_py_per_infection_averted, probs = 0.975)) %>%
  ggplot(aes(x = scenario, y = prep_py_per_infection_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = prep_py_per_infection_averted_lower,
                    ymax = prep_py_per_infection_averted_upper),
                position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = "none") +
  labs(x = "Scenario", y = "Person-years on PrEP per infection averted"))

## Person-time on PrEP per infection averted, by sex
print(prep_persontime_age_sex_risk %>%
  group_by(scenario, Gender, run_num) %>%
  summarise(prep_py = sum(prep_py)) %>%
  left_join(new_infections_total, by = c("run_num", "scenario")) %>%
  group_by(Gender, run_num) %>%
  mutate(base_case_infections = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case_infections),
         prep_py_per_infection_averted = prep_py/infections_averted) %>%
  group_by(scenario, Gender) %>%
  summarise(prep_py_per_infection_averted_mean = mean(prep_py_per_infection_averted),
            prep_py_per_infection_averted_lower = quantile(prep_py_per_infection_averted, probs = 0.025),
            prep_py_per_infection_averted_upper = quantile(prep_py_per_infection_averted, probs = 0.975)) %>%
  ggplot(aes(x = Gender, y = prep_py_per_infection_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = prep_py_per_infection_averted_lower,
                    ymax = prep_py_per_infection_averted_upper),
                position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Scenario", y = "Person-years on PrEP per infection averted"))

## Person-time on PrEP per infection averted, by sex and risk group
print(prep_persontime_age_sex_risk %>%
  group_by(scenario, Gender, risk_group, run_num) %>%
  summarise(prep_py = sum(prep_py)) %>%
  left_join(new_infections_total, by = c("run_num", "scenario")) %>%
  group_by(Gender, risk_group, run_num) %>%
  mutate(base_case_infections = new_infections[scenario == "base_case"]) %>%
  filter(scenario != "base_case") %>%
  mutate(infections_averted = abs(new_infections - base_case_infections),
         prep_py_per_infection_averted = prep_py/infections_averted) %>%
  group_by(scenario, Gender, risk_group) %>%
  summarise(prep_py_per_infection_averted_mean = mean(prep_py_per_infection_averted),
            prep_py_per_infection_averted_lower = quantile(prep_py_per_infection_averted, probs = 0.025),
            prep_py_per_infection_averted_upper = quantile(prep_py_per_infection_averted, probs = 0.975)) %>%
  ggplot(aes(x = risk_group, y = prep_py_per_infection_averted_mean, group = scenario)) +
  geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = prep_py_per_infection_averted_lower,
                    ymax = prep_py_per_infection_averted_upper),
                position = position_dodge(0.9)) +
  scale_fill_viridis_d(begin = 1/3, guide = guide_legend(title = "Scenario")) +
  labs(x = "Scenario", y = "Person-years on PrEP per infection averted") +
  facet_wrap(~Gender))

dev.off()

