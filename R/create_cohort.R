################################################################################
## Allen Roberts
## Create incidence cohort for relationships analysis
################################################################################

## Libraries
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(RColorBrewer)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Plot settings
theme_set(theme_classic())

## Run information
suite_name <- "relationships-test2"
scale_factor <- 0.01

## Cohort settings
min_cohort_year <- 2015
max_cohort_year <- 2025
min_cohort_age <- 15
max_cohort_age <- 35

## Report Event Recorder
## Determine age, gender, death, and HIV infection status for each individual
## Construct cohort of individuals eligible for HIV incidence rate analysis
event_recorder <- read.csv(file.path("output", suite_name, "ReportEventRecorder.csv"), stringsAsFactors = FALSE)

event_recorder <- event_recorder %>%
  mutate(age = Age/365,
         gender = ifelse(Gender == "M", "Men", "Women"),
         risk = factor(Risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))) %>%
  rename("year" = "Year",
         "id" = "Individual_ID",
         "infected" = "Infected",
         "event" = "Event_Name")

cohort <- event_recorder %>%
  select(id, gender, age, year, event, infected) %>%
  filter(event != "PropertyChange") %>%
  filter(year <= max_cohort_year) %>%
  filter(age >= min_cohort_age & floor(age) <= max_cohort_age) %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(has_death = any(event %in% c("NonDiseaseDeaths", "DiseaseDeaths")),
         death_date = ifelse(has_death == TRUE, year[event %in% c("NonDiseaseDeaths", "DiseaseDeaths")], NA),
         death_age = ifelse(has_death == TRUE, age[event %in% c("NonDiseaseDeaths", "DiseaseDeaths")], NA)) %>%
  filter(is.na(death_date) | death_date > 2015) %>%
  mutate(initial_hiv_status = ifelse(first(infected == 0), "Negative", "Positive"),
         year_exit_age = max_cohort_age - age + year) %>%
  filter(!any(year_exit_age <= min_cohort_year)) %>%
  filter(initial_hiv_status == "Negative") %>%
  mutate(reaches_age_max = ifelse(floor(age) > max_cohort_age, TRUE, FALSE),
         ever_infected = any(event == "NewInfectionEvent"),
         infection_date = ifelse(ever_infected == TRUE, max(year[event == "NewInfectionEvent"]), NA),
         infection_age = ifelse(ever_infected == TRUE, age[event == "NewInfectionEvent"], NA)) %>%
  filter(is.na(infection_date) | infection_date >= min_cohort_year) %>%
  mutate(entry_date = pmax(min_cohort_year, min(year)),
         entry_age = median(age + (entry_date - year)),
         exit_date = ifelse(ever_infected == TRUE, pmin(infection_date, year_exit_age), pmin(year_exit_age, max_cohort_year, death_date, na.rm = TRUE)),
         exit_age = entry_age + exit_date - entry_date) %>%
  mutate(ever_infected = ifelse(ever_infected == TRUE & infection_date > exit_date + 1e-2, FALSE, ever_infected),
         infection_date = ifelse(ever_infected == FALSE, NA, infection_date)) %>%
  mutate(exit_age = median(exit_age),
         exit_date = median(exit_date)) %>%
  select(id, gender, entry_date, entry_age, exit_date, exit_age, ever_infected, infection_date) %>%
  rename("ends_with_infection" = "ever_infected") %>%
  distinct() %>%
  filter(abs(exit_date-entry_date) > 1/365) %>% ## Gets rid of people who are infected on the same day as their eligibility date
  mutate(py = exit_age - entry_age) %>%
  ungroup()

stopifnot(length(unique(cohort$id)) == nrow(cohort))

## Cohort descriptive statistics
## Note that some individuals hit age 35, but their exit date is within days of
## their birthday. Can't seem to get this exactly right from the output, 
## but it's close enough
cohort %>%
  summarise(start_year = min(entry_date),
            end_year = max(exit_date),
            start_age = min(entry_age),
            end_age = max(entry_age + py),
            n = length(unique(id)),
            min_py = min(py),
            median_py = median(py),
            max_py = max(py))

cohort %>%
  group_by(gender) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100))

inc_gender_plot <- cohort %>%
  group_by(gender) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100)) %>%
  ggplot(aes(x = gender, y = ir)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Incidence rate (per 100 PY)") +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
  ggtitle("HIV incidence rate, 2015-2025")

## Add risk status
## FIXME: Still a problem here. Person-years is lower in cohort_risk than in cohort
cohort_risk <- event_recorder %>%
  select(id, event, gender, age, year, risk) %>%
  left_join(cohort %>% select(id, entry_date, entry_age, exit_date, exit_age, infection_date), by = c("id")) %>%
  filter(age >= (entry_age - 1e-2) & age <= (exit_age + 1e-2)) %>%
  arrange(id, age) %>%
  group_by(id) %>%
  mutate(min_age = min(age),
         start_age = ifelse(age == min_age, entry_age, age),
         start_year = ifelse(age == min_age, entry_date, year),
         end_age = lead(age),
         end_year = lead(year),
         end_age = ifelse(is.na(end_age), exit_age, end_age),
         end_year = ifelse(is.na(end_year), exit_date, end_year),
         ends_with_infection = ifelse(infection_date == end_year & event == "NewInfectionEvent", 1, 0),
         ends_with_infection = ifelse(is.na(infection_date), 0, ends_with_infection),
         end_age = ifelse(end_age < start_age, start_age, end_age),
         py = end_age - start_age) 

## Check to make sure that the cohorts are consistent
## There are very small differences in person-years that are bascially result of floating point errors and rounding
## Going to ignore for now. 
cohort_ids <- cohort %>%
  filter(ends_with_infection == TRUE) %>%
  select(id) %>%
  distinct() %>%
  pull()

cohort_risk_ids <- cohort_risk %>%
  filter(ends_with_infection == TRUE) %>%
  select(id) %>%
  distinct() %>%
  pull()

ids <- cohort_ids[!(cohort_ids %in% cohort_risk_ids)]
stopifnot(length(ids) == 0)

ids <- cohort_risk_ids[!(cohort_risk_ids %in% cohort_ids)]
stopifnot(length(ids) == 0)

cohort %>%
  group_by(gender) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100))

cohort_risk %>%
  group_by(gender) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100))

# cohort_py <- cohort %>% 
#   group_by(id) %>%
#   summarise(py = sum(py))
# 
# cohort_risk_py <- cohort_risk %>% 
#   group_by(id) %>%
#   summarise(py = sum(py))
# 
# ids <- cohort_py$id[cohort_py$py != cohort_risk_py$py]

## Incidence rate by risk
cohort_risk %>%
  group_by(gender, risk) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100))

inc_gender_risk_plot <- cohort_risk %>%
  group_by(gender, risk) %>%
  summarise(n = length(unique(id)),
            num_infections = sum(ends_with_infection),
            py = sum(py)) %>%
  mutate(ir = num_infections/(py/100)) %>%
  ggplot(aes(x = risk, y = ir)) +
  geom_bar(aes(fill = fct_rev(risk)), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(direction = -1, name = "Risk") +
  labs(x = "Risk", y = "Incidence rate (per 100 PY)") +
  facet_wrap(~gender) +
  ggtitle("HIV incidence rate, 2015-2025")

## Save plots
pdf(file = file.path("output", suite_name, "incidence_plots.pdf"), height = 6, width = 8)
print(inc_gender_plot)
print(inc_gender_risk_plot)
dev.off()

## Save output
save(cohort,
     cohort_risk,
     scale_factor,
     suite_name,
     file = file.path("output", suite_name, "incidence_cohort.RData"))
