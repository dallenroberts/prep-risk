################################################################################
## Allen Roberts
## Determine person-time and incidence rates in relationships
################################################################################
## Libraries
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(RColorBrewer)
library(data.table)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot settings
theme_set(theme_classic())

## Run information
suite_name <- "relationships-test2"
scale_factor <- 0.01
base_year <- 1960.5

## Cohort settings
min_cohort_year <- 2015
max_cohort_year <- 2025
min_cohort_age <- 15
max_cohort_age <- 35

## Load incidence cohort and transmissions datasets
load(file.path("output", suite_name, "incidence_cohort.RData"))
load(file.path("output", suite_name, "transmissions.RData"))

## Create ID for cohort intervals in cohort_risk
cohort_risk <- cohort_risk %>%
  arrange(id, start_age) %>%
  group_by(id) %>%
  mutate(int_id = row_number()) %>%
  relocate(int_id, .after = id)

## Read in relationships datasets
rel_start <- read.csv(file.path("output", suite_name, "RelationshipStart.csv"), stringsAsFactors = FALSE)
rel_end <- read.csv(file.path("output", suite_name, "RelationshipEnd.csv"), stringsAsFactors = FALSE)

## Relationship Start
rel_start <- rel_start %>%
  rename("rel_id" = "Rel_ID",
         "rel_start_time" = "Rel_start_time",
         "rel_type" = "Rel_type..0...TRANSITORY..1...INFORMAL..2...MARITAL..3...COMMERCIAL.",
         "a_id" = "A_ID",
         "a_startage" = "A_age",
         "a_infected" = "A_is_infected",
         "b_id" = "B_ID",
         "b_startage" = "B_age",
         "b_infected" = "B_is_infected") %>%
  mutate(rel_start_year = base_year + rel_start_time/365,
         rel_type = factor(rel_type, levels = c(0, 1, 2, 3), labels = c("Transitory", "Informal", "Marital", "Commercial")),
         a_gender = ifelse(A_gender == 0, "Men", "Women"),
         b_gender = ifelse(B_gender == 0, "Men", "Women"),
         a_risk = gsub("Risk-", "", grep("Risk", str_split(A_IndividualProperties, pattern = ";", simplify = TRUE), value = TRUE)),
         a_risk = factor(a_risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High")),
         b_risk = gsub("Risk-", "", grep("Risk", str_split(B_IndividualProperties, pattern = ";", simplify = TRUE), value = TRUE)),
         b_risk = factor(b_risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))) %>%
  select(rel_id,
         rel_type,
         rel_start_time,
         rel_start_year,
         a_id,
         a_gender,
         a_startage,
         a_risk,
         a_infected,
         b_id,
         b_gender,
         b_startage,
         b_risk,
         b_infected) %>%
  filter(rel_start_year <= max_cohort_year)

## Relationship End
## Ages - use this to help sort out exact times
rel_end_ages <- rel_end %>%
  rename("rel_id" = "Rel_ID") %>%
  select(rel_id, male_ID, female_ID, male_age, female_age) %>%
  pivot_longer(cols = -rel_id, names_to = c("gender", ".value"), names_sep = "_") %>%
  rename("id" = "ID",
         "end_age" = "age") %>%
  select(id, rel_id, end_age)

rel_end <- rel_end %>%
  rename("rel_id" = "Rel_ID",
         "rel_end_time" = "Rel_actual_end_time") %>%
  select(rel_id, rel_end_time) %>%
  mutate(rel_end_year = base_year + rel_end_time/365)

## Merge
rel <- rel_start %>%
  left_join(rel_end, by = "rel_id")


## Distribution of relationship duration (among all relationships, across all years)
## First censor relationships that haven't ended using the last observed end_time
rel_duration <- rel %>%
  mutate(rel_end_time = ifelse(is.na(rel_end_time), max(rel_end_time, na.rm = TRUE), rel_end_time),
         rel_duration_days = rel_end_time - rel_start_time)

rel_duration %>%
  group_by(rel_type) %>%
  summarise(mean = mean(rel_duration_days),
            median = median(rel_duration_days),
            lower = quantile(rel_duration_days, probs = 0.25),
            upper = quantile(rel_duration_days, probs = 0.75),
            min = min(rel_duration_days),
            max = max(rel_duration_days))


## Reshape to individual-level
rel_ind <- rel %>%
  mutate(a_partnerinfected = ifelse(b_infected == 1, 1, 0),
         b_partnerinfected = ifelse(a_infected == 1, 1, 0)) %>%
  pivot_longer(cols = starts_with(c("a_", "b_")), names_to = c("ind", ".value"), names_sep = "_") %>%
  select(-ind) %>%
  left_join(rel_end_ages, by = c("id", "rel_id")) %>%
  rename("partner_infected" = "partnerinfected",
         "rel_start_age" = "startage",
         "rel_end_age" = "end_age") %>%
  mutate(rel_end_year = ifelse(is.na(rel_end_year), 2100, rel_end_year),
         rel_end_age = ifelse(is.na(rel_end_age), 120, rel_end_age)) %>%
  filter(rel_end_year > min_cohort_year & rel_start_age < max_cohort_age & rel_end_age > min_cohort_age)

## Only look at relationships that started when the individual was uninfected, for individuals in the cohort
rel_ind <- rel_ind %>%
  filter(infected == 0 & id %in% cohort_risk$id)

## Calculate person-time in a relationship, among person-time reflected in the incidence cohort
## Use the foverlaps() function to determine, for each relationship in rel_ind,
## which (if any) of the cohort_risk intervals it overlaps with
rel_ind <- as.data.table(rel_ind)
cohort_risk <- as.data.table(cohort_risk)

rel_ind <- rel_ind %>%
  mutate(rel_start_age_days = rel_start_age * 365,
         rel_end_age_days = rel_end_age * 365)
setkey(rel_ind, id, rel_start_age, rel_end_age)

cohort_risk <- cohort_risk %>%
  mutate(start_age_days = start_age * 365,
         end_age_days = end_age * 365)
setkey(cohort_risk, id, start_age_days, end_age_days)

rel_pt <- foverlaps(x = rel_ind %>% select(-c(gender, risk)), y = cohort_risk,
          by.x = c("id", "rel_start_age_days", "rel_end_age_days"),
          by.y = c("id", "start_age_days", "end_age_days")) 

## A small fraction of relationships get excluded because they land on the entry/exit timestep (for either age or year). But relationship reporter thinks they are eligible. Excluding these for now. Could go back and rework this to create discrete timesteps (based on integer values), but would need to meet with Dan B to make sure I'm handling this correctly.
sum(is.na(rel_pt$event))

rel_pt <- rel_pt %>%
  filter(!is.na(event)) %>%
  mutate(py_in_rel_start = pmax(start_age_days, rel_start_age_days)/365,
         py_in_rel_end = pmin(end_age_days, rel_end_age_days)/365,
         py_in_rel = py_in_rel_end - py_in_rel_start)

## Verify that all relationships that end with a transmission in the incidence cohort are captured here.
rel_pt_ids <- unique(rel_pt$rel_id)
trans_ids <- unique(inc_trans$rel_id)

stopifnot(sum(!(trans_ids %in% rel_pt_ids)) == 0)

## Person-time in a relationship
## Need to first collapse overlapping intervals for individuals in multiple relationships
## Motivated by this solution:
## https://stackoverflow.com/questions/41747742/collapse-rows-with-overlapping-ranges
rel_pt_by_int <- rel_pt %>%
  select(id, int_id, rel_start_age_days, rel_end_age_days) %>%
  group_by(id, int_id) %>%
  arrange(id, int_id, rel_start_age_days) %>%
  group_by(id, int_id, g = cumsum(cummax(lag(rel_end_age_days, default = first(rel_end_age_days))) <= rel_start_age_days)) %>%
  summarise(rel_start_age_days = first(rel_start_age_days), 
            rel_end_age_days = max(rel_end_age_days))

## Merge back to cohort_risk intervals
rel_pt_by_int <- rel_pt_by_int %>%
  inner_join(cohort_risk %>% select(id, int_id, start_age_days, end_age_days), by = c("id", "int_id")) %>%
  mutate(py_in_rel_start = pmax(start_age_days, rel_start_age_days)/365,
         py_in_rel_end = pmin(end_age_days, rel_end_age_days)/365,
         py_in_rel = py_in_rel_end - py_in_rel_start) %>%
  group_by(id, int_id) %>%
  summarise(py_in_rel = sum(py_in_rel)) %>%
  right_join(cohort_risk, by = c("id", "int_id")) 

## Calculate descriptive statistics
## Person-time while in a relationship
## Incidence rate while in a relationship
rel_pt_by_int %>%
  group_by(gender) %>%
  summarise(py_in_rel = sum(py_in_rel, na.rm = TRUE),
            total_py = sum(py),
            num_inf = sum(ends_with_infection)) %>%
  mutate(prop_in_rel = py_in_rel/total_py,
         ir = num_inf/py_in_rel)

rel_pt_by_int %>%
  group_by(gender, risk) %>%
  summarise(py_in_rel = sum(py_in_rel, na.rm = TRUE),
            total_py = sum(py),
            num_inf = sum(ends_with_infection)) %>%
  mutate(prop_in_rel = py_in_rel/total_py,
         ir = num_inf/py_in_rel)

## Verify py calculation looks right by comparing denominator with cohort_risk
cohort_risk %>%
  group_by(gender, risk) %>%
  summarise(total_py = sum(py),
            num_inf = sum(ends_with_infection)) %>%
  mutate(ir = num_inf/total_py)

## Person-time in specific relationships
rel_pt_by_int_type <- rel_pt %>%
  select(id, int_id, rel_type, rel_start_age_days, rel_end_age_days) %>%
  group_by(id, int_id, rel_type) %>%
  arrange(id, int_id, rel_type, rel_start_age_days) %>%
  group_by(id, int_id, rel_type, g = cumsum(cummax(lag(rel_end_age_days, default = first(rel_end_age_days))) <= rel_start_age_days)) %>%
  summarise(rel_start_age_days = first(rel_start_age_days), 
            rel_end_age_days = max(rel_end_age_days))

## Merge back to cohort_risk intervals
rel_pt_by_int_type <- rel_pt_by_int_type %>%
  inner_join(cohort_risk %>% select(id, int_id, start_age_days, end_age_days), by = c("id", "int_id")) %>%
  mutate(py_in_rel_start = pmax(start_age_days, rel_start_age_days)/365,
         py_in_rel_end = pmin(end_age_days, rel_end_age_days)/365,
         py_in_rel = py_in_rel_end - py_in_rel_start) %>%
  group_by(id, int_id, rel_type) %>%
  summarise(py_in_rel = sum(py_in_rel)) %>%
  right_join(cohort_risk, by = c("id", "int_id")) 

## Summarise person-time
rel_pt_by_int_type_summary <- rel_pt_by_int_type %>%
  filter(!is.na(rel_type)) %>%
  group_by(gender, risk, rel_type) %>%
  summarise(py_in_rel = sum(py_in_rel))

## Summarise infections and merge
rel_pt_by_int_type_summary <- inc_trans %>%
  group_by(dest_gender, dest_risk, rel_type) %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  summarise(num_infections = n()) %>%
  right_join(rel_pt_by_int_type_summary, by = c("gender", "risk", "rel_type")) %>%
  mutate(num_infections = ifelse(is.na(num_infections), 0, num_infections)) %>%
  mutate(ir = num_infections/py_in_rel)

## Summarise total person-time at risk and merge
rel_pt_by_int_type_summary <- cohort_risk %>%
  group_by(gender, risk) %>%
  summarise(total_py = sum(py)) %>%
  right_join(rel_pt_by_int_type_summary, by = c("gender", "risk")) %>%
  mutate(prop_py_in_rel = py_in_rel/total_py)

rel_pt_by_int_type_summary %>%
  group_by(gender, rel_type) %>%
  summarise(num_infections = sum(num_infections),
            py_in_rel = sum(py_in_rel),
            total_py = sum(total_py)) %>%
  mutate(ir = num_infections/py_in_rel,
         prop_py_in_rel = py_in_rel/total_py)

## Now, instead of using the cohort-interval level, do the analysis on the relationship level
## Total relationship-time at risk, by gender and risk
rel_rt_summary <-  rel_pt %>%
  group_by(gender, risk) %>%
  summarise(rel_pt = sum(py_in_rel))

rel_rt_summary <- inc_trans %>%
  group_by(dest_gender, dest_risk) %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  summarise(num_infections = n()) %>%
  inner_join(rel_rt_summary, by = c("gender", "risk")) %>%
  mutate(ir = num_infections/rel_pt)
  

## Total relationship-time at risk, by gender and risk and relationship type
rel_rt_by_type_summary <- rel_pt %>%
  group_by(gender, risk, rel_type) %>%
  summarise(rel_pt = sum(py_in_rel)) %>%
  group_by(gender, risk) %>%
  mutate(prop_pt = rel_pt/sum(rel_pt))

rel_rt_by_type_summary <- inc_trans %>%
  group_by(dest_gender, dest_risk, rel_type) %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  summarise(num_infections = n()) %>%
  inner_join(rel_rt_by_type_summary, by = c("gender", "risk", "rel_type")) %>%
  mutate(ir = num_infections/rel_pt)

## OTHER QUESTIONS
## What percent of transmissions occur in relationships that started when the other partner was already infected?
trans_by_initial_partner_status <- inc_trans %>%
  select(rel_id, dest_id, dest_gender, dest_risk) %>%
  left_join(rel_start, by = "rel_id") %>%
  mutate(partner_infected = ifelse(dest_id == a_id, b_infected, a_infected)) %>%
  select(rel_id, rel_type, dest_id, dest_gender, dest_risk, partner_infected) %>%
  rename("gender" = "dest_gender",
         "risk" = "dest_risk")

trans_by_initial_partner_status%>%
  group_by(gender) %>%
  summarise(num_partner_initially_infected = sum(partner_infected),
            num_transmission = n()) %>%
  mutate(pct_partner_initially_infected = num_partner_initially_infected/num_transmission)

trans_by_initial_partner_status%>%
  group_by(gender, risk) %>%
  summarise(num_partner_initially_infected = sum(partner_infected),
            num_transmission = n()) %>%
  mutate(pct_partner_initially_infected = num_partner_initially_infected/num_transmission)

trans_by_initial_partner_status%>%
  group_by(gender, rel_type) %>%
  summarise(num_partner_initially_infected = sum(partner_infected),
            num_transmission = n()) %>%
  mutate(pct_partner_initially_infected = num_partner_initially_infected/num_transmission)

trans_by_initial_partner_status%>%
  group_by(gender, risk, rel_type) %>%
  summarise(num_partner_initially_infected = sum(partner_infected),
            num_transmission = n()) %>%
  mutate(pct_partner_initially_infected = num_partner_initially_infected/num_transmission)


## What's the average time from relationship start to infection?
## For all relationships that lead to infection
time_to_transmission <- inc_trans %>%
  select(rel_id, dest_id, dest_gender, dest_risk, time) %>%
  rename("transmission_time" = "time") %>%
  left_join(rel_start, by = "rel_id") %>%
  mutate(partner_infected = ifelse(dest_id == a_id, b_infected, a_infected),
         days_to_infection = transmission_time - rel_start_time) %>%
  select(rel_id, rel_type, dest_id, dest_gender, dest_risk, partner_infected, days_to_infection) %>%
  rename("gender" = "dest_gender",
         "risk" = "dest_risk")
  
time_to_transmission %>%
  group_by(gender) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))

time_to_transmission %>%
  group_by(gender, risk) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))

time_to_transmission %>%
  group_by(gender, rel_type) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))

## For relationships in which the partner was already infected? 
time_to_transmission %>%
  group_by(gender, partner_infected) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))

time_to_transmission %>%
  group_by(gender, risk, partner_infected) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))

time_to_transmission %>%
  group_by(gender, rel_type, partner_infected) %>%
  summarise(n = n(),
            mean = mean(days_to_infection),
            median = median(days_to_infection),
            lower = quantile(days_to_infection, probs = 0.25),
            upper = quantile(days_to_infection, probs = 0.75),
            min = min(days_to_infection),
            max = max(days_to_infection),
            prop_more_than_1mo = mean(days_to_infection > 30))
  
## What is the incidence rate in relationships, for ones that start with an HIV-negative partner and ones that start with an HIV-positive partner?
rel_rt_by_type_infected_summary <- rel_pt %>%
  group_by(gender, risk, rel_type, partner_infected) %>%
  summarise(rel_pt = sum(py_in_rel)) %>%
  group_by(gender, risk) %>%
  mutate(prop_pt = rel_pt/sum(rel_pt))

rel_rt_by_type_infected_summary <- inc_trans %>%
  left_join(rel %>% select(rel_id, a_id, b_id, a_infected, b_infected), by = c("rel_id")) %>%
  mutate(partner_infected = ifelse(dest_id == a_id, b_infected, a_infected)) %>%
  group_by(dest_gender, dest_risk, rel_type, partner_infected) %>%
  summarise(num_infections = n()) %>%
  rename("gender" = "dest_gender", "risk" = "dest_risk") %>%
  inner_join(rel_rt_by_type_infected_summary, by = c("gender", "risk", "rel_type", "partner_infected")) %>%
  mutate(ir = num_infections/rel_pt)


## OLD NOTES

## Collapse overlapping intervals to count overall person-time in a relationship
## Motivated by this solution:
## https://stackoverflow.com/questions/41747742/collapse-rows-with-overlapping-ranges
# test <- test %>%
#   group_by(id) %>%
#   arrange(id, start_age) %>%
#   group_by(id, g = cumsum(cummax(lag(end_age, default = first(end_age))) < start_age)) %>%
#   summarise(start_age = first(start_age), end_age = max(end_age)) %>%
#   group_by(id) %>%
#   summarise(py_in_rel = end_age - start_age)
# 
# test %>% 
#   group_by(gender, risk) %>%
#   summarise(py_in_rel = sum(py_in_rel))
# 
# cohort_risk %>%
#   group_by(gender, risk) %>%
#   summarise(total_py = sum(py))
# 
# 
# rel_pt <- rel_ind %>%
#   filter(infected == 0) %>%
#   inner_join(cohort, by = c("id", "gender")) %>%
#   filter(rel_start_age < exit_age & rel_end_age > entry_age) %>%
#   mutate(start_age = pmax(rel_start_age, entry_age),
#          end_age = pmin(rel_end_age, exit_age)) %>%
#   select(id, gender, risk, rel_id, rel_type, partner_infected, start_age, end_age, ends_with_infection) %>%
#   arrange(id, start_age, end_age)
# 
# ## Whether the relationship ended in a transmission
# rel_trans <- inc_trans %>%
#   select(dest_id, rel_id, year, dest_age) %>%
#   rename("id" = "dest_id",
#          "transmission_year" = "year",
#          "transmission_age" = "dest_age")
# 
# ## CONTINUE FROM HERE
# ## One issue is that multiple relationships are denoted with ends_with_infection == 1, but they aren't necessarily the ones in which a transmission occurred. This is happening (correctly) because the person-time at risk is ending due to a transmission occurring (and not due to aging or time), but the transmission isn't occuring in this specific relationship (it's occuring in another). So I should clarify this variable name, and be sure to use the ended_with_transmission indicator for incidence within that relationship.
# 
# rel_pt <- rel_pt %>%
#   left_join(rel_trans, by = c("id", "rel_id")) %>%
#   mutate(ended_with_transmission = ifelse(!is.na(transmission_year), 1, 0)) 
# 
# ## Verify that all infections are accounted for
# rel_pt %>%
#   group_by(id) %>%
#   mutate(has_transmission = sum(ended_with_transmission)) %>%
#   mutate(has_infection = any(ends_with_infection == 1)) %>%
#   filter(has_infection == 1 & has_transmission == 0)
# 
# test <- rel_pt %>%
#   select(-ends_with_infection) 
# 
# ## Collapse overlapping intervals to count overall person-time in a relationship
# ## Motivated by this solution:
# ## https://stackoverflow.com/questions/41747742/collapse-rows-with-overlapping-ranges
# test <- test %>%
#   group_by(id) %>%
#   arrange(id, start_age) %>%
#   group_by(id, g = cumsum(cummax(lag(end_age, default = first(end_age))) < start_age)) %>%
#   summarise(start_age = first(start_age), end_age = max(end_age)) %>%
#   group_by(id) %>%
#   summarise(py_in_rel = end_age - start_age)
# 
# ## Things to ask:
# ## What percent of person-time is spent in a relationship?
# test %>%
#   right_join(cohort, by = "id") %>%
#   mutate(prop_py_in_rel = ifelse(is.na(py_in_rel), 0, py_in_rel/py)) %>%
#   ungroup() %>%
#   summarise(mean = mean(prop_py_in_rel),
#             median = median(prop_py_in_rel),
#             iqr_lower = quantile(prop_py_in_rel, probs = 0.25),
#             iqr_upper = quantile(prop_py_in_rel, probs = 0.75),
#             min = min(prop_py_in_rel),
#             max = max(prop_py_in_rel))
# 
# ## Stratified by gender
# test %>%
#   right_join(cohort, by = "id") %>%
#   mutate(prop_py_in_rel = ifelse(is.na(py_in_rel), 0, py_in_rel/py)) %>%
#   group_by(gender) %>%
#   summarise(mean = mean(prop_py_in_rel),
#             median = median(prop_py_in_rel),
#             iqr_lower = quantile(prop_py_in_rel, probs = 0.25),
#             iqr_upper = quantile(prop_py_in_rel, probs = 0.75),
#             min = min(prop_py_in_rel),
#             max = max(prop_py_in_rel))

## Stratified by risk group
## To do this, need a cohort where a new line occurs when the following events happen:
## Risk changes
## Relationship starts
## Relationship ends

## This could get large very quickly. I wonder if I could approach this differently.
## Construct separate cohorts for each gender/age/risk combination
## Calculate whatever statistics I need on that cohort
## Would need to refact all the previous code to take age limits, risk, gender as arugments


## What's the distribution of relationship time, by relationship type?
  ## Might want to look at this for a longer period - eg, not just in the incidence cohort
## What percent of transmissions occur in relationships that started when the other partner was already infected?
## What's the average time from relationship start to infection?
  ## For all relationships that lead to infection, and for relationships in which the partner was already infected? 
## What is the incidence rate in relationships, in general and by relationship type?
## What is the incidence rate in relationships, for ones that start with an HIV-negative partner and ones that start with an HIV-positive partner?

