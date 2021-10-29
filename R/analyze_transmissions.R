################################################################################
## Allen Roberts
## Determine fraction of transmissions that occur, by relationship type
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

## Read in TransmissionReport
trans_report <- read.csv(file.path("output", suite_name, "TransmissionReport.csv"), stringsAsFactors = FALSE)

## Load incidence cohort
load(file.path("output", suite_name, "incidence_cohort.RData"))

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
               select(id), by = c("dest_id" = "id"))

## Verify that we have the same number
stopifnot(nrow(inc_trans) == sum(cohort$ends_with_infection))

summary(inc_trans$dest_age)

## Transmissions, by gender and destination risk group
gender_drisk_plot <- inc_trans %>%
  group_by(dest_gender, dest_risk) %>%
  summarise(n = n()) %>%
  group_by(dest_gender) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = dest_gender, y = pct)) +
    geom_col(aes(fill = fct_rev(dest_risk))) +
    scale_fill_viridis_d(name = "Dest risk", direction = -1) +
    labs(x = "Dest gender", y = "Proportion") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggtitle("HIV transmissions, 2015-2025")

## Transmissions, by gender, source and destination risk group
gender_drisk_srisk_plot <- inc_trans %>%
  group_by(dest_gender, dest_risk, src_risk) %>%
  summarise(n = n()) %>%
  group_by(dest_gender) %>%
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = dest_risk, y = pct)) +
  geom_col(aes(fill = fct_rev(src_risk))) +
  scale_fill_viridis_d(name = "Source risk", direction = -1) +
  labs(x = "Dest risk", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~dest_gender) +
  ggtitle("HIV transmissions, 2015-2025")

gender_drisk_srisk_scaled_plot <- inc_trans %>%
  group_by(dest_gender, dest_risk, src_risk) %>%
  summarise(n = n()) %>%
  group_by(dest_gender, dest_risk) %>%
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = dest_risk, y = pct)) +
  geom_col(aes(fill = fct_rev(src_risk))) +
  scale_fill_viridis_d(name = "Source risk", direction = -1) +
  labs(x = "Dest risk", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~dest_gender) +
  ggtitle("HIV transmissions, 2015-2025")

## Transmissions, by gender, destination risk group, and relationship type
gender_drisk_rel_plot <- inc_trans %>%
  group_by(dest_gender, dest_risk, rel_type) %>%
  summarise(n = n()) %>%
  group_by(dest_gender) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x = dest_risk, y = pct)) +
  geom_col(aes(fill = fct_rev(rel_type))) +
  scale_fill_viridis_d(name = "Relationship type", direction = -1) +
  labs(x = "Dest risk", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~dest_gender) +
  ggtitle("HIV transmissions, 2015-2025")

## Look at just young women ages 15-34
## Note that age is numeric, so age == 35 is 35.00
## By risk group
yw_drisk_plot <- inc_trans %>%
  filter(dest_gender == "Women" & dest_age >= 15 & dest_age <= 35) %>%
  group_by(dest_risk) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n),
         dest_gender = "Women",
         dest_age = "16-35") %>%
  ggplot(aes(x = dest_gender, y = pct)) +
  geom_col(aes(fill = fct_rev(dest_risk))) +
  scale_fill_viridis_d(name = "Dest risk", direction = -1) +
  labs(x = "Dest gender", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("HIV transmissions, 2015-2025, among women 15-34")

## By relationship type
yw_rel_plot <- inc_trans %>%
  filter(dest_gender == "Women" & dest_age >= 15 & dest_age <= 35) %>%
  group_by(rel_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n),
         dest_gender = "Women",
         dest_age = "16-35") %>%
  ggplot(aes(x = dest_gender, y = pct)) +
  geom_col(aes(fill = fct_rev(rel_type))) +
  scale_fill_viridis_d(name = "Relationship type", direction = -1) +
  labs(x = "Dest gender", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("HIV transmissions, 2015-2025, among women 15-34")

## By risk group and relationship type
yw_drisk_rel_plot <- inc_trans %>%
  filter(dest_gender == "Women" & dest_age >= 15 & dest_age <= 35) %>%
  group_by(dest_risk, rel_type) %>%
  summarise(n = n()) %>%
  group_by(dest_risk) %>%
  mutate(pct = n/sum(n),
         dest_gender = "Women",
         dest_age = "16-35") %>%
  ggplot(aes(x = dest_risk, y = pct)) +
  geom_col(aes(fill = fct_rev(rel_type))) +
  scale_fill_viridis_d(name = "Relationship type", direction = -1) +
  labs(x = "Dest gender", y = "Proportion") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("HIV transmissions, 2015-2025, among women 15-34")

## Save plots
pdf(file = file.path("output", suite_name, "transmissions_plot.pdf"), height = 6, width = 8)
print(gender_drisk_plot)
print(gender_drisk_srisk_plot)
print(gender_drisk_srisk_scaled_plot)
print(gender_drisk_rel_plot)
print(yw_drisk_plot)
print(yw_rel_plot)
print(yw_drisk_rel_plot)
dev.off()
    
## Save dataset
save(inc_trans,
     all_trans,
     scale_factor,
     suite_name,
     file = file.path("output", suite_name, "transmissions.RData"))
