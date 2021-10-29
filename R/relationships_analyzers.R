################################################################################
## Allen Roberts
## Functions for relationships analyses
################################################################################
## Libraries
library(tidyverse)
library(data.table)

## Suppress unhelpful warnings
options(dplyr.summarise.inform = FALSE) 

## Create incidence cohort and summaries
create_cohort <- function(suite_name = "relationships_5",
                          campaign_name = "Baseline-campaign_orig-base_case",
                          tpi = "TPI0001", 
                          min_cohort_year = 2015,
                          max_cohort_year = 2025,
                          min_cohort_age = 15,
                          max_cohort_age = 35) {
  
  ## Generate file path to Report_Event_Recorder
  ff <- file.path("output", suite_name, campaign_name, "ReportEventRecorder", paste0("ReportEventRecorder_", tpi, "_REP0099.csv"))
    
  ## Generate run number
  run_num <- as.integer(gsub("TPI", "", tpi))
  
  ## Load Report_Event_Recorder
  event_recorder <- read.csv(ff, stringsAsFactors = FALSE)
  
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
    filter(is.na(death_date) | death_date > min_cohort_year) %>%
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
  
  ## Compute incidence rates
  cohort_ir <- cohort %>%
    group_by(gender) %>%
    summarise(n = length(unique(id)),
              num_infections = sum(ends_with_infection),
              py = sum(py)) %>%
    mutate(ir = num_infections/py,
           run_num = run_num)
  
  cohort_risk_ir <- cohort_risk %>%
    group_by(gender, risk) %>%
    summarise(n = length(unique(id)),
              num_infections = sum(ends_with_infection),
              py = sum(py)) %>%
    mutate(ir = num_infections/py,
           run_num = run_num)
  
  ## Save and return
  outputs <- list(
    "cohort" = cohort,
    "cohort_risk" = cohort_risk,
    "cohort_ir" = cohort_ir,
    "cohort_risk_ir" = cohort_risk_ir
  )
  
  return(outputs)

}

## Create datasets of transmissions that occur in incidence cohort
create_transmissions_dataset <- function(suite_name = "relationships_5",
                                         campaign_name = "Baseline-campaign_orig-base_case",
                                         tpi = "TPI0001",
                                         cohort,
                                         min_cohort_year = 2015,
                                         max_cohort_year = 2025,
                                         min_cohort_age = 15,
                                         max_cohort_age = 35) {
  
  ## Generate file path to Transmission Report
  ff <- file.path("output", suite_name, campaign_name, "TransmissionReport", paste0("TransmissionReport_", tpi, "_REP0099.csv"))
  
  ## Generate run number
  run_num <- as.integer(gsub("TPI", "", tpi))
  
  ## Read in TransmissionReport
  trans_report <- read.csv(ff, stringsAsFactors = FALSE)
  
  ## Format transmission report
  trans_report <- trans_report %>%
    rename("time" = "SIM_TIME",
           "year" = "YEAR",
           "rel_id" = "REL_ID",
           "src_id" = "SRC_ID",
           "dest_id" = "DEST_ID",
           "rel_type" = "REL_TYPE..0...TRANSITORY..1...INFORMAL..2...MARITAL..3...COMMERCIAL.") %>%
    mutate(rel_type = factor(rel_type, levels = c(0, 1, 2, 3), labels = c("Transitory", "Informal", "Marital", "Commercial")),
           src_gender = ifelse(SRC_GENDER == 0, "Men", "Women"),
           src_age = SRC_AGE/365,
           src_risk = gsub("Risk:", "", grep("Risk", str_split(SRC_IP, pattern = ",", simplify = TRUE), value = TRUE)),
           src_risk = factor(src_risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High")),
           dest_gender = ifelse(DEST_GENDER == 0, "Men", "Women"),
           dest_age = DEST_AGE/365,
           dest_risk = gsub("Risk:", "", grep("Risk", str_split(DEST_IP, pattern = ",", simplify = TRUE), value = TRUE)),
           dest_risk = factor(dest_risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))) %>%
    select(time, year, rel_id, rel_type, src_id, src_gender, src_age, src_risk, dest_id, dest_gender, dest_age, dest_risk)
  
  ## All transmissions - use to establish time of HIV-positivity
  all_trans <- trans_report %>%
    select(time, year, dest_id)
  
  ## Only look at transmission that occur between 2015 and 2025, in the incidence cohort
  inc_trans <- trans_report %>%
    filter(year >= min_cohort_year & year <= max_cohort_year) %>%
    right_join(cohort %>% 
                 filter(ends_with_infection == TRUE) %>%
                 select(id), by = c("dest_id" = "id")) %>%
    mutate(run_num = run_num)
  
  ## Verify that we have the same number
  stopifnot(nrow(inc_trans) == sum(cohort$ends_with_infection))
  
  ## Transmissions summaries
  ## Transmissions, by gender and destination risk group
  gender_drisk <- inc_trans %>%
    group_by(dest_gender, dest_risk) %>%
    summarise(n = n()) %>%
    group_by(dest_gender) %>%
    mutate(prop = n/sum(n),
           run_num = run_num)
    
  ## Transmissions, by gender, source and destination risk group
  gender_drisk_srisk <- inc_trans %>%
    group_by(dest_gender, dest_risk, src_risk) %>%
    summarise(n = n()) %>%
    group_by(dest_gender) %>%
    mutate(prop = n/sum(n),
           run_num = run_num)
  
  gender_drisk_srisk_scaled <- inc_trans %>%
    group_by(dest_gender, dest_risk, src_risk) %>%
    summarise(n = n()) %>%
    group_by(dest_gender, dest_risk) %>%
    mutate(prop = n/sum(n),
           run_num = run_num)
  
  ## Transmissions, by gender, destination risk group, and relationship type
  gender_drisk_rel <- inc_trans %>%
    group_by(dest_gender, dest_risk, rel_type) %>%
    summarise(n = n()) %>%
    group_by(dest_gender) %>%
    mutate(prop = n/sum(n),
           run_num = run_num) 
  
  
  return(list(
    "inc_trans" = inc_trans,
    "trans_by_gender_drisk" = gender_drisk,
    "trans_by_gender_drisk_srisk" = gender_drisk_srisk,
    "trans_by_gender_drisk_srisk_scaled" = gender_drisk_srisk_scaled,
    "trans_by_gender_drisk_rel" = gender_drisk_rel
    )
  )
}

## Create relationships results
analyze_relationships <- function(suite_name = "relationships_5",
                                  campaign_name = "Baseline-campaign_orig-base_case",
                                  tpi = "TPI0001",
                                  cohort_risk,
                                  inc_trans,
                                  min_cohort_year = 2015,
                                  max_cohort_year = 2025,
                                  min_cohort_age = 15,
                                  max_cohort_age = 35,
                                  base_year = 1960.5) {
  
  ## Generate run number
  run_num <- as.integer(gsub("TPI", "", tpi))
  
  ## Load relationship start and relationships end
  rel_start <- read.csv(file.path("output", suite_name, campaign_name, "RelationshipStart", paste0("RelationshipStart_", tpi, "_REP0099.csv")), stringsAsFactors = FALSE)
  
  rel_end <- read.csv(file.path("output", suite_name, campaign_name, "RelationshipEnd", paste0("RelationshipEnd_", tpi, "_REP0099.csv")), stringsAsFactors = FALSE)
  
  ## Create ID for cohort intervals in cohort_risk
  cohort_risk <- cohort_risk %>%
    arrange(id, start_age) %>%
    group_by(id) %>%
    mutate(int_id = row_number()) %>%
    relocate(int_id, .after = id)
  
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
  
  rel_duration_summary <- rel_duration %>%
    group_by(rel_type) %>%
    summarise(mean = mean(rel_duration_days),
              median = median(rel_duration_days),
              lower = quantile(rel_duration_days, probs = 0.25),
              upper = quantile(rel_duration_days, probs = 0.75),
              min = min(rel_duration_days),
              max = max(rel_duration_days)) %>%
    mutate(run_num = run_num)
  
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
  
  ## A small fraction of relationships get excluded because they land on the entry/exit timestep (for either age or year). But relationship reporter thinks they are eligible. Excluding these for now since they are very rare. Could go back and rework this to create discrete timesteps (based on integer values), but would need to meet with Dan B to make sure I'm handling this correctly.
  sum(is.na(rel_pt$event))
  
  rel_pt <- rel_pt %>%
    filter(!is.na(event)) 

  ## Annoyingly, an occasional transmission is missed because the precision of the cohort_risk year variable is less than the precision in the relationships variable, and rounding makes them not overlap. This is a messy but needed fix to add those relationships back in
  missed_transmissions <-  rel_ind %>%
    filter(rel_id %in% unique(inc_trans$rel_id) & !(rel_id %in% unique(rel_pt$rel_id)))
  
  if(nrow(missed_transmissions) > 0) {
    
    missed_rels <- cohort_risk %>%
      filter(id %in% missed_transmissions$id & event == "NewInfectionEvent") %>%
      left_join(missed_transmissions %>% select(-c(gender, risk)), by = c("id"))
    
    rel_pt <- rbind(rel_pt, missed_rels)
    
    
  }  
  
  rel_pt <- rel_pt %>%
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
  rel_pt_by_int_summary <- rel_pt_by_int %>%
    group_by(gender, risk) %>%
    summarise(py_in_rel = sum(py_in_rel, na.rm = TRUE),
              total_py = sum(py),
              num_inf = sum(ends_with_infection)) %>%
    mutate(prop_in_rel = py_in_rel/total_py,
           ir = num_inf/py_in_rel,
           run_num = run_num)
  
  ## Verify py calculation looks right by comparing denominator with cohort_risk
  # cohort_risk %>%
  #   group_by(gender, risk) %>%
  #   summarise(total_py = sum(py),
  #             num_inf = sum(ends_with_infection)) %>%
  #   mutate(ir = num_inf/total_py)
  
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
  
  rel_pt_by_int_type_summary <- rel_pt_by_int_type_summary %>%
    group_by(gender, risk, rel_type) %>%
    summarise(num_infections = sum(num_infections),
              py_in_rel = sum(py_in_rel),
              total_py = sum(total_py)) %>%
    mutate(ir = num_infections/py_in_rel,
           prop_py_in_rel = py_in_rel/total_py,
           run_num = run_num)
  
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
    mutate(ir = num_infections/rel_pt,
           run_num = run_num)
  
  
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
    mutate(ir = num_infections/rel_pt,
           run_num = run_num)
  
  ## OTHER QUESTIONS
  ## What percent of transmissions occur in relationships that started when the other partner was already infected?
  trans_by_initial_partner_status <- inc_trans %>%
    select(rel_id, dest_id, dest_gender, dest_risk) %>%
    left_join(rel_start, by = "rel_id") %>%
    mutate(partner_infected = ifelse(dest_id == a_id, b_infected, a_infected)) %>%
    select(rel_id, rel_type, dest_id, dest_gender, dest_risk, partner_infected) %>%
    rename("gender" = "dest_gender",
           "risk" = "dest_risk")
  
  trans_by_initial_partner_status_summary <- trans_by_initial_partner_status%>%
    group_by(gender, risk, rel_type) %>%
    summarise(num_partner_initially_infected = sum(partner_infected),
              num_transmission = n()) %>%
    mutate(pct_partner_initially_infected = num_partner_initially_infected/num_transmission,
           run_num = run_num)
  
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
  
  time_to_transmission_gender <- time_to_transmission %>%
    group_by(gender) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
  time_to_transmission_gender_risk <- time_to_transmission %>%
    group_by(gender, risk) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
  time_to_transmission_gender_rel_type <- time_to_transmission %>%
    group_by(gender, rel_type) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
  ## For relationships in which the partner was already infected? 
  time_to_transmission_gender_pi <- time_to_transmission %>%
    group_by(gender, partner_infected) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
  time_to_transmission_gender_risk_pi <- time_to_transmission %>%
    group_by(gender, risk, partner_infected) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
  time_to_transmission_gender_rel_pi <- time_to_transmission %>%
    group_by(gender, rel_type, partner_infected) %>%
    summarise(n = n(),
              mean = mean(days_to_infection),
              median = median(days_to_infection),
              lower = quantile(days_to_infection, probs = 0.25),
              upper = quantile(days_to_infection, probs = 0.75),
              min = min(days_to_infection),
              max = max(days_to_infection),
              prop_more_than_1mo = mean(days_to_infection > 30)) %>%
    mutate(run_num = run_num)
  
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
    right_join(rel_rt_by_type_infected_summary, by = c("gender", "risk", "rel_type", "partner_infected")) %>%
    mutate(num_infections = replace(num_infections, is.na(num_infections), 0)) %>%
    mutate(ir = num_infections/rel_pt,
           run_num = run_num)
  
  ## Return desired outputs
  return(list(
    "rel_duration_summary" = rel_duration_summary,
    "rel_pt_by_int_summary" = rel_pt_by_int_summary,
    "rel_pt_by_int_type_summary" = rel_pt_by_int_type_summary,
    "rel_rt_summary" = rel_rt_summary,
    "rel_rt_by_type_summary" = rel_rt_by_type_summary,
    "trans_by_initial_partner_status_summary" = trans_by_initial_partner_status_summary,
    "time_to_transmission_gender" = time_to_transmission_gender,
    "time_to_transmission_gender_risk" = time_to_transmission_gender_risk,
    "time_to_transmission_gender_rel_type" = time_to_transmission_gender_rel_type,
    "time_to_transmission_gender_pi" = time_to_transmission_gender_pi,
    "time_to_transmission_gender_risk_pi" = time_to_transmission_gender_risk_pi,
    "time_to_transmission_gender_rel_pi" = time_to_transmission_gender_rel_pi,
    "rel_rt_by_type_infected_summary" = rel_rt_by_type_infected_summary
  ))
  
}


