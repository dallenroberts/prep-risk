################################################################################
## Allen Roberts
## Compare HIV incidence and prevalence with differing assumptions on max number
## of clients CSWs can have
################################################################################

rm(list = ls())

## Libraries
library(ggplot2)
library(tidyverse)
library(binom)
library(viridisLite)

theme_set(theme_classic())

## Model results
load(file.path("output", "maxclients_sweep_fonly", "sweep_results.RData"))

## Data
inc_data <- read.csv(file.path("Data", "observed", "incidence.csv"), stringsAsFactors = FALSE)
prev_data <- read.csv(file.path("Data", "observed", "prevalence.csv"), stringsAsFactors = FALSE)

## Subset to adults
inc_data <- inc_data %>%
  filter(gender != "Both") %>%
  filter(age_lower %in% c(15, 18) & age_upper == 49) %>%
  mutate(Gender = ifelse(gender == "Female", "Women", "Men")) %>%
  select(-gender) %>%
  rename(Year = year)

prev_data <- prev_data %>%
  filter(gender != "Both") %>%
  filter(age_lower %in% c(15,18) & age_upper == 49) %>%
  mutate(Gender = ifelse(gender == "Female", "Women", "Men")) %>%
  select(-gender) %>%
  rename(Year = year)

## Estimate CIs when not provided
prev_data <- prev_data %>%
  mutate(effective_sample_size = sample_size/deff,
         effective_cases = prevalence*effective_sample_size)

prev_data$lower[is.na(prev_data$lower)] <- with(prev_data[is.na(prev_data$lower),], binom.confint(x = effective_cases,
                                                              n = effective_sample_size,
                                                              methods = "exact")$lower)
prev_data$upper[is.na(prev_data$upper)] <- with(prev_data[is.na(prev_data$upper),], binom.confint(x = effective_cases,
                                                                                                  n = effective_sample_size,
                                                                                                  methods = "exact")$upper)

## Plots
pdf(file = file.path("figures", "calibration_fit_by_client_sweep_fonly.pdf"), height = 6, width = 8)

## Incidence among high risk women
print(inc_adults_sex_risk %>%
        filter(Gender == "Women" & risk_group == "High") %>%
        rename(incidence = mean_inc) %>%
        ggplot(aes(x = Year, y = incidence)) +
        geom_line(aes(color = factor(clients_female))) +
        geom_ribbon(aes(ymin = lower_inc, ymax = upper_inc, fill = factor(clients_female)), alpha = 0.1, show.legend = FALSE) +
        scale_color_viridis_d(direction = -1, guide = guide_legend(title = "Max clients for women", reverse = TRUE)) +
        # scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
        ggtitle("Incidence among high risk adult women"))


## Incidence among adult women
print(inc_adults_sex %>%
  filter(Gender == "Women") %>%
  rename(incidence = mean_inc) %>%
  ggplot(aes(x = Year, y = incidence)) +
    geom_line(aes(color = factor(clients_female))) +
    geom_ribbon(aes(ymin = lower_inc, ymax = upper_inc, fill = factor(clients_female)), alpha = 0.1, show.legend = FALSE) +
    geom_point(data = inc_data[inc_data$Gender == "Women",], aes(x = Year, y = incidence, shape = source)) +
    geom_errorbar(data = inc_data[inc_data$Gender == "Women",], aes(ymin = lower, ymax = upper), width = 2) +
    scale_color_viridis_d(direction = -1, guide = guide_legend(title = "Max clients for women", reverse = TRUE)) +
    scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
    ggtitle("Incidence among adult women"))

## Incidence among adult men
print(inc_adults_sex %>%
  filter(Gender == "Men") %>%
  rename(incidence = mean_inc) %>%
  ggplot(aes(x = Year, y = incidence)) +
  geom_line(aes(color = factor(clients_female))) +
  geom_ribbon(aes(ymin = lower_inc, ymax = upper_inc, fill = factor(clients_female)), alpha = 0.1, show.legend = FALSE) +
  geom_point(data = inc_data[inc_data$Gender == "Men",], aes(x = Year, y = incidence, shape = source)) +
  geom_errorbar(data = inc_data[inc_data$Gender == "Men",], aes(ymin = lower, ymax = upper), width = 2) +
  scale_color_viridis_d(direction = -1, guide = guide_legend(title = "Max clients for women", reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
  ggtitle("Incidence among adult men"))

## Prevalence among adult women
print(prev_adults_sex %>%
  filter(Gender == "Women") %>%
  rename(prevalence = mean_prev) %>%
  ggplot(aes(x = Year, y = prevalence)) +
  geom_line(aes(color = factor(clients_female))) +
  geom_ribbon(aes(ymin = lower_prev, ymax = upper_prev, fill = factor(clients_female)), alpha = 0.1, show.legend = FALSE) +
  geom_point(data = prev_data[prev_data$Gender == "Women",], aes(x = Year, y = 100*prevalence, shape = source)) +
  geom_errorbar(data = prev_data[prev_data$Gender == "Women",], aes(ymin = 100*lower, ymax = 100*upper), width = 2) +
  scale_color_viridis_d(direction = -1, guide = guide_legend(title = "Max clients for women", reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  ggtitle("Prevalence among adult women"))


## Prevalence among adult men
print(prev_adults_sex %>%
  filter(Gender == "Men") %>%
  rename(prevalence = mean_prev) %>%
  ggplot(aes(x = Year, y = prevalence)) +
  geom_line(aes(color = factor(clients_female))) +
  geom_ribbon(aes(ymin = lower_prev, ymax = upper_prev, fill = factor(clients_female)), alpha = 0.1, show.legend = FALSE) +
  geom_point(data = prev_data[prev_data$Gender == "Men",], aes(x = Year, y = 100*prevalence, shape = source)) +
  geom_errorbar(data = prev_data[prev_data$Gender == "Men",], aes(ymin = 100*lower, ymax = 100*upper), width = 2) +
  scale_color_viridis_d(direction = -1, guide = guide_legend(title = "Max clients for women", reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  ggtitle("Prevalence among adult men"))

dev.off()

