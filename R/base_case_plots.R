##############################################################################
## Allen Roberts
## Plot results from base case (no PrEP) simulation
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
library(lemon)

theme_set(theme_classic())

options(dplyr.summarise.inform=F)
numCores <- detectCores()

file_list <- list.files("test-output", pattern = "report|base_case")

load(file.path("test-output", "Baseline-campaign_orig-base_case", "ReportHIVByAgeAndGender", "output.RData"))

## Percentage in risk group over time
pdf(file = file.path("figures", "base_case_plots.pdf"),
    height = 5, width = 7)
print(outputs$pop_adults_sex_risk %>%
  group_by(Gender, Year, run_num) %>%
  mutate(pct = pop/sum(pop)) %>%
  group_by(Gender, risk_group, Year) %>%
  summarise(pop_pct_mean = mean(pct),
            pop_pct_lower = quantile(pct, probs = 0.025),
            pop_pct_upper = quantile(pct, probs = 0.975)) %>%
  ggplot(aes(x = Year, y = 100*pop_pct_mean, color = risk_group)) +
  geom_line() +
  geom_ribbon(aes(ymin = 100*pop_pct_lower, ymax = 100*pop_pct_upper, fill = risk_group), alpha = 0.4) +
  scale_color_viridis_d(guide = guide_legend(title = "Risk group")) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = "Year", y = "% of population") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  facet_wrap(~Gender))

print(outputs$new_infections_age_sex_risk_year %>%
        group_by(Gender, Year, risk_group, run_num) %>%
        summarise(new_infections = sum(new_infections)) %>%
        group_by(Gender, risk_group, Year) %>%
        summarise(new_infections_mean = mean(new_infections),
                  new_infections_lower = quantile(new_infections, probs = 0.025),
                  new_infections_upper = quantile(new_infections, probs = 0.975)) %>%
        ggplot(aes(x = Year, y = new_infections_mean, color = risk_group)) +
        geom_line() +
        geom_ribbon(aes(ymin = new_infections_lower, ymax = new_infections_upper, fill = risk_group), alpha = 0.4) +
        scale_color_viridis_d(guide = guide_legend(title = "Risk group")) +
        scale_fill_viridis_d(guide = "none") +
        labs(x = "Year", y = "Annual new infections") +
        facet_wrap(~Gender))

dev.off()


