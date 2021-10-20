################################################################################
## Allen Roberts
## Plots and tables of relationship analysis results
################################################################################

## Libraries
library(tidyverse)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(viridisLite)
library(viridis)
library(scales)

## Run information
suite_name <- "relationships_25"
scale_factor <- 0.01

## Cohort settings
min_cohort_year <- 2022
max_cohort_year <- 2031
min_cohort_age <- 15
max_cohort_age <- 35

## Load results
load(file.path("output", suite_name, "relationships_results.RData"))

## Incidence rate by gender
outputs$cohort_ir %>%
  mutate(ir = 100*ir) %>%
  group_by(gender) %>%
  summarise(median = median(ir),
            lower = quantile(ir, probs = 0.25),
            upper = quantile(ir, probs = 0.75))

## Percentage of person-time, by gender and risk
outputs$cohort_risk_ir %>%
  group_by(gender, run_num) %>%
  mutate(prop_py = py/sum(py)) %>%
  group_by(gender, risk) %>%
  summarise(median = median(prop_py),
            lower = quantile(prop_py, probs = 0.25),
            upper = quantile(prop_py, probs = 0.75))

## Incidence rate by gender and risk
outputs$cohort_risk_ir %>%
  mutate(ir = 100*ir) %>%
  group_by(gender, risk) %>%
  summarise(median = median(ir),
            lower = quantile(ir, probs = 0.25),
            upper = quantile(ir, probs = 0.75))

## Relationship duration, by relationship type
outputs$rel_duration_summary %>%
  group_by(rel_type) %>%
  summarise(median_months = median(median)/30.42,
            lower_months = median(lower)/30.42,
            upper_months = median(upper)/30.42)

## Proportion of transmissions, by gender and relationship type
outputs$trans_by_gender_drisk_rel %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  group_by(gender, rel_type) %>%
  summarise(n = sum(n)) %>%
  group_by(gender) %>%
  mutate(prop = n/sum(n))

## With uncertainty
outputs$trans_by_gender_drisk_rel %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  group_by(run_num, gender, rel_type) %>%
  summarise(n = sum(n)) %>%
  group_by(run_num, gender) %>%
  mutate(prop = n/sum(n)) %>%
  group_by(gender, rel_type) %>%
  summarise(median = median(prop),
            lower = quantile(prop, probs = 0.25),
            upper= quantile(prop, probs = 0.75))
  

## Proportion of transmissions, by gender, risk, and relationship type
outputs$trans_by_gender_drisk_rel %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  group_by(gender, risk, rel_type) %>%
  summarise(n = sum(n)) %>%
  group_by(gender, risk) %>%
  mutate(prop = n/sum(n))

## With uncertainty
outputs$trans_by_gender_drisk_rel %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  group_by(run_num, gender, risk, rel_type) %>%
  summarise(n = sum(n)) %>%
  group_by(run_num, gender, risk) %>%
  mutate(prop = n/sum(n)) %>%
  group_by(gender, risk, rel_type) %>%
  summarise(median = median(prop),
            lower = quantile(prop, probs = 0.25),
            upper= quantile(prop, probs = 0.75))

## Proportion of person-time in a relationship, by gender and risk
outputs$rel_pt_by_int_summary %>%
  mutate(prop_py_in_rel = py_in_rel/total_py) %>%
  group_by(gender, risk) %>%
  summarise(median = median(prop_py_in_rel),
            lower = quantile(prop_py_in_rel, probs = 0.25),
            upper = quantile(prop_py_in_rel, probs = 0.75))

## Proportion of relationship time that occurs with HIV-positive partners, by gender and risk
outputs$rel_rt_by_type_infected_summary %>%
  group_by(gender, risk, partner_infected, run_num) %>%
  summarise(rel_pt = sum(rel_pt)) %>%
  group_by(gender, risk, run_num) %>%
  mutate(prop_rel_pt_infected = rel_pt/sum(rel_pt)) %>%
  group_by(gender, risk, partner_infected) %>%
  summarise(median = median(prop_rel_pt_infected),
            lower = quantile(prop_rel_pt_infected, probs = 0.25),
            upper = quantile(prop_rel_pt_infected, probs = 0.75))


## Incidence rate while in relationship, by gender and risk
outputs$rel_pt_by_int_summary %>%
  mutate(ir = 100*ir) %>%
  group_by(gender, risk) %>%
  summarise(median = median(ir),
            lower = quantile(ir, probs = 0.25),
            upper = quantile(ir, probs = 0.75))

## Incidence rate in relationships with HIV+ partner (infected at the start of the relationship)
outputs$rel_rt_by_type_infected_summary %>%
  # filter(risk %in% c("Low", "Medium")) %>%
  group_by(run_num, gender, risk, partner_infected) %>%
  summarise(num_infections = sum(num_infections),
            rel_pt = sum(rel_pt)) %>%
  mutate(ir = 100*num_infections/rel_pt) %>%
  group_by(gender, risk, partner_infected) %>%
  summarise(median = median(ir),
            lower = quantile(ir, probs = 0.25),
            upper = quantile(ir, probs = 0.75))

## Proportion of transmissions that occur in relationships where the HIV+ partner was infected at the start of the relationship
outputs$trans_by_initial_partner_status_summary %>%
  group_by(run_num, gender, risk) %>%
  summarise(num_partner_initially_infected = sum(num_partner_initially_infected),
            num_transmission = sum(num_transmission)) %>%
  mutate(prop_partner_initially_infected = num_partner_initially_infected/num_transmission) %>%
  group_by(gender, risk) %>%
  summarise(median = median(prop_partner_initially_infected),
            lower = quantile(prop_partner_initially_infected, probs = 0.25),
            upper = quantile(prop_partner_initially_infected, probs = 0.75))

## Time from relationship start to transmission
outputs$time_to_transmission_gender_risk_pi %>%
  group_by(gender, risk, partner_infected) %>%
  summarise(median_months = median(median)/30.42,
            lower_months = median(lower)/30.42,
            upper_months = median(upper)/30.42)
  
  