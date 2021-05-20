################################################################################
## Allen Roberts
## Compare HIV incidence and prevalence with differing assumptions on max number
## of clients CSWs can have
################################################################################

rm(list = ls())

## Libraries
library(ggplot2)
library(tidyverse)
library(lemon)
library(viridisLite)

theme_set(theme_classic())

options(dplyr.summarise.inform=F)

clients_female <- c(59, seq(6, 12, by = 1))
# clients_male <- seq(2, 58, by = 4)

for(ff in clients_female) {
  # for(mm in clients_male) {
    
    cat("\n\n\n")
    cat(paste("clients_female:", ff))
    cat("\n")
    # cat(paste("clients_male:", mm))
    # cat("\n")
    
    # file_path <- paste0("test-output/Baseline-campaign_orig-base_case_maxsimrelfemales", ff, "_maxsimrelmales", mm, "/ReportHIVByAgeAndGender/")
    file_path <- file.path("test-output", paste0("Baseline-campaign_orig-base_case_maxsimrelfemales", ff), "ReportHIVByAgeAndGender")
    
    if(!file.exists(file.path(file_path, "output.RData"))) {
      print("No outputs")
    } else {
      
      load(file.path(file_path, "output.RData"))
      
      ## Prevalence in adults by sex
      prev_s <- outputs$prev_adults_sex %>%
        group_by(Gender, Year) %>%
        summarize(mean_prev = mean(prev),
                  median_prev = median(prev),
                  lower_prev = quantile(prev, probs = 0.025),
                  upper_prev = quantile(prev, probs = 0.975))
      # prev_s$clients_male <- mm
      prev_s$clients_female <- ff
      
      ## Prevalence by age and sex
      prev_as <- outputs$prev_age_sex %>%
        group_by(Gender, age_group, Year) %>%
        summarize(mean_prev = mean(prev),
                  median_prev = median(prev),
                  lower_prev = quantile(prev, probs = 0.025),
                  upper_prev = quantile(prev, probs = 0.975))
      # prev_as$clients_male <- mm
      prev_as$clients_female <- ff
      
      ## Prevalence among adults by sex and risk
      prev_asr <- outputs$prev_adults_sex_risk %>%
        group_by(Gender, risk_group, Year) %>%
        summarize(mean_prev = mean(prev),
                  median_prev = median(prev),
                  lower_prev = quantile(prev, probs = 0.025),
                  upper_prev = quantile(prev, probs = 0.975))
      # prev_asr$clients_male <- mm
      prev_asr$clients_female <- ff
      
      ## Incidence in adults, by sex
      inc_s <- outputs$inc_adults_sex %>%
        group_by(Gender, Year) %>%
        summarize(mean_inc = mean(inc),
                  median_inc = median(inc),
                  lower_inc = quantile(inc, probs = 0.025),
                  upper_inc = quantile(inc, probs = 0.975))
      # inc_s$clients_male <- mm
      inc_s$clients_female <- ff
      
      ## Incidence by age and sex
      inc_as <- outputs$inc_age_sex %>%
        group_by(Gender, age_group, Year) %>%
        summarize(mean_inc = mean(inc),
                  median_inc = median(inc),
                  lower_inc = quantile(inc, probs = 0.025),
                  upper_inc = quantile(inc, probs = 0.975))
      # inc_as$clients_male <- mm
      inc_as$clients_female <- ff
      
      ## Incidence among adults by sex and risk
      inc_asr <- outputs$inc_adults_sex_risk %>%
        group_by(Gender, risk_group, Year) %>%
        summarize(mean_inc = mean(inc),
                  median_inc = median(inc),
                  lower_inc = quantile(inc, probs = 0.025),
                  upper_inc = quantile(inc, probs = 0.975))
      # inc_asr$clients_male <- mm
      inc_asr$clients_female <- ff
      
      # if(ff == clients_female[1] & mm == clients_male[1]) {
      if(ff == clients_female[1]) {  
        ## Create data frames
        prev_adults_sex <- prev_s
        prev_age_sex <- prev_as
        prev_adults_sex_risk <- prev_asr
        inc_adults_sex <- inc_s
        inc_age_sex <- inc_as
        inc_adults_sex_risk <- inc_asr
        
      } else {
        
        ## Append
        prev_adults_sex <- rbind(prev_adults_sex, prev_s)
        prev_age_sex <- rbind(prev_age_sex, prev_as)
        prev_adults_sex_risk <- rbind(prev_adults_sex_risk, prev_asr)
        inc_adults_sex <- rbind(inc_adults_sex, inc_s)
        inc_age_sex <- rbind(inc_age_sex, inc_as)
        inc_adults_sex_risk <- rbind(inc_adults_sex_risk, inc_asr)
        
      }
      
    }
    
    
  # }
}

## Save
save(prev_adults_sex, prev_age_sex, prev_adults_sex_risk, inc_adults_sex, inc_age_sex, inc_adults_sex_risk, file = "output/maxclients_sweep_fonly/sweep_results.RData")

## Plot
pdf(file = file.path("output/maxclients_sweep_fonly/inc_prev_sweep_plots.pdf"), height = 5, width = 7)

## Incidence
for(gender in c("Women", "Men")) {
  
  print(inc_adults_sex %>%
          filter(Gender == gender) %>%
          ggplot(aes(x = Year, y = mean_inc, color = factor(clients_female))) +
          geom_line(aes(group = clients_female), alpha = 0.8) +
          labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
          scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE)) +
          scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
          # facet_rep_wrap(~clients_male, repeat.tick.labels = TRUE) +
          ggtitle(paste("HIV incidence, ages 15-49,", gender)))

  for(rg in c("High", "Medium", "Low")) {
    
    print(inc_adults_sex_risk %>%
      filter(Gender == gender & risk_group == rg & Year >= 2010) %>%
      ggplot(aes(x = Year, y = mean_inc, color = factor(clients_female))) +
      geom_line(aes(group = clients_female), alpha = 0.8) +
      labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
      scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE)) +
      scale_x_continuous(limits = c(2010, 2060), breaks = seq(2010, 2060, by = 20)) +
      # facet_rep_wrap(~clients_male, repeat.tick.labels = TRUE) +
      ggtitle(paste("HIV incidence, ages 15-54,", rg, "risk", gender)))
  }
}

## Prevalence
for(gender in c("Women", "Men")) {
  
  ## In general poppulation
  print(prev_adults_sex %>%
          filter(Gender == gender) %>%
          ggplot(aes(x = Year, y = mean_prev, color = factor(clients_female))) +
          geom_line(aes(group = clients_female), alpha = 0.8) +
          labs(x = "Year", y = "Prevalence (%)") +
          scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE)) +
          scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
          # facet_rep_wrap(~clients_male, repeat.tick.labels = TRUE) +
          ggtitle(paste("HIV prevalence, ages 15-49,", gender)))
  
  for(rg in c("High", "Medium", "Low")) {
    
    print(prev_adults_sex_risk %>%
            filter(Gender == gender & risk_group == rg & Year >= 2010) %>%
            ggplot(aes(x = Year, y = mean_prev, color = factor(clients_female))) +
            geom_line(aes(group = clients_female), alpha = 0.8) +
            labs(x = "Year", y = "Prevalence (%)") +
            scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE)) +
            scale_x_continuous(limits = c(2010, 2060), breaks = seq(2010, 2060, by = 20)) +
            # facet_rep_wrap(~clients_male, repeat.tick.labels = TRUE) +
            ggtitle(paste("HIV prevalence, ages 15-54,", rg, "risk", gender)))
  }
}

dev.off()
