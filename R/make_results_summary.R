################################################################################
## Allen Roberts
## Function to generate summary plots (and potentially tabular output) from EMOD experiments
## Time series of population, prevalence, incidence, and intervention coverage 
## Various stratifications by age, sex, and risk group
## For now, simply saves a .pdf called "plots" in a given output directory
## Running this function will also delete older results that are located in the directory
################################################################################

make_results_summary <- function(name, report_interval = 0.5, make_plots = TRUE) {
  
  ## Libraries
  library(ggplot2)
  library(tidyverse)
  library(lemon)
  library(parallel)
  library(foreach)
  library(doParallel)
  
  ## Set up parallels
  numCores <- detectCores()
  registerDoParallel(numCores)
  
  
  ## Plot settings
  theme_set(theme_classic())
  
  file_path <- paste("test-output", name, "ReportHIVByAgeAndGender", sep = "/")
  file_list <- list.files(file_path, pattern = ".csv", full.names = TRUE)
  
  ## Remove previous files
  too_old <- -60*60 ## Delete files created this many seconds or more than the most recent file created
  file_times <- file.info(file_list)$mtime
  file.remove(file_list[file_times - max(file_times) <= too_old])
  file_list <- list.files(file_path, pattern = ".csv")
  
  ## Function to combine results across different runs
  comb <- function(x, ...) {  
    mapply(rbind,x,...,SIMPLIFY=FALSE)
  }
  
  outputs <- foreach(i=1:length(file_list), .combine = 'comb', .multicombine = TRUE) %dopar% {
    
    ## Libraries
    library(ggplot2)
    library(tidyverse)
    library(lemon)
    library(parallel)
    library(foreach)
    library(doParallel)
    
    print(i)
    ff <- file_list[i]
    run_num <- as.integer(gsub("[^0-9]", "", unlist(strsplit(ff, "_"))[2]))
    df <- read.csv(file.path(file_path, ff), stringsAsFactors = FALSE)
    df$run_num <- run_num
    
    ## 
    df$age_group <- cut(df$Age, breaks = c(-Inf, 14, 24, 34, 44, 54, 64, Inf), labels = c("< 15", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
    df$Gender <- factor(df$Gender, levels = c(0,1), labels = c("Men", "Women"))
    df$risk_group <- factor(df$IP_Key.Risk, levels = c("LOW", "MEDIUM", "HIGH"), labels = c("Low", "Medium", "High"))
    
    ## Total population over time
    pop <- (df %>%
              group_by(Year, run_num) %>%
              summarise(pop = sum(Population))
    )
    
    ## Population size by age/sex/risk
    pop_age_sex_risk <- (df %>%
                           group_by(Gender, age_group, risk_group, Year, run_num) %>%
                           summarise(pop = sum(Population))
    )
    
    ## Population size, 15-54 by sex and risk
    pop_adults_sex_risk <- (df %>%
                              filter(Age >= 15 & Age <= 54) %>%
                              group_by(Gender, risk_group, Year, run_num) %>%
                              summarise(pop = sum(Population))
    )
    
    ## Prevalence, ages 15-49, by sex
    prev_adults_sex <- (df %>%
                          filter(Age >= 15 & Age <= 49) %>%
                          group_by(Gender, Year, run_num) %>%
                          summarise(prev = 100*sum(Infected)/sum(Population))) 
    
    ## Prevalence - age/sex
    prev_age_sex <- (df %>% 
                       group_by(Gender, age_group, Year, run_num) %>%
                       summarise(prev = 100*sum(Infected)/sum(Population))
    )
    
    ## Prevalence - age/sex/risk
    prev_age_sex_risk <- (df %>% 
                            group_by(Gender, age_group, risk_group, Year, run_num) %>%
                            summarise(prev = 100*sum(Infected)/sum(Population))
    )
    
    ## Prevalence, 15-54, by sex and risk
    prev_adults_sex_risk <- (df %>%
                               filter(Age >= 15 & Age <= 54) %>%
                               group_by(Gender, risk_group, Year, run_num) %>%
                               summarise(prev = 100*sum(Infected)/sum(Population))
    )
    
    ## Start and end of time interval for incidence calculation
    df$year_start <- df$Year - report_interval
    df$year_end <- df$Year ## not needed, but just to make it explicit
    
    ## One-year intervals corresponding to calendar year during which new infections/person-time were accrued
    df$year_floor <- floor(df$year_start)
    
    ## Person-time at risk
    df$person_years_at_risk = ifelse(df$HasHIV == 0, df$Population*report_interval, df$Newly.Infected*report_interval/2)
    
    ## Incidence - age/sex
    inc_age_sex <- (df %>% 
                      group_by(Gender, age_group, year_floor, run_num) %>%
                      summarise(new_infections = sum(Newly.Infected),
                                person_years = sum(person_years_at_risk)) %>%
                      mutate(inc = 100*new_infections/person_years,
                             Year = year_floor)
    )
    
    ## Incidence - age/sex/risk
    inc_age_sex_risk <- (df %>% 
                           group_by(Gender, age_group, risk_group, year_floor, run_num) %>%
                           summarise(new_infections = sum(Newly.Infected),
                                     person_years = sum(person_years_at_risk)) %>%
                           mutate(inc = 100*new_infections/person_years,
                                  Year = year_floor)
    )
    
    ## Incidence, 15-54, by sex and risk
    inc_adults_sex_risk <- (df %>%
                              filter(Age >= 15 & Age <= 54) %>%
                              group_by(Gender, risk_group, year_floor, run_num) %>%
                              summarise(new_infections = sum(Newly.Infected),
                                        person_years = sum(person_years_at_risk)) %>%
                              mutate(inc = 100*new_infections/person_years,
                                     Year = year_floor)
    )
    
    ## Incidence, adults 15-49, by sex 
    inc_adults_sex <- (df %>%
                         filter(Age >= 15 & Age <= 49) %>%
                         group_by(Gender, year_floor, run_num) %>%
                         summarise(new_infections = sum(Newly.Infected),
                                   person_years = sum(person_years_at_risk)) %>%
                         mutate(inc = 100*new_infections/person_years,
                                Year = year_floor))
    
    ## ART coverage - sex 
    art_sex <- (df %>% 
                  group_by(Gender, Year, run_num) %>%
                  summarise(art_cov = 100 * sum(On_ART)/sum(Infected))
    )
    
    ## ART coverage, 15-54, by sex and risk
    art_adults_sex_risk <- (df %>%
                              filter(Age >= 15 & Age <= 54) %>%
                              group_by(Gender, risk_group, Year, run_num) %>%
                              summarise(art_cov = 100 * sum(On_ART)/sum(Infected))
    )
    
    ## PrEP coverage - age and sex
    prep_age_sex <- (df %>% 
                       group_by(Gender, age_group, Year, run_num) %>%
                       summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
    )
    
    ## PrEP coverage - age, sex, and risk
    prep_age_sex_risk <- (df %>% 
                            group_by(Gender, age_group, risk_group, Year, run_num) %>%
                            summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
    )
    
    ## PrEP coverage, 15-54, by sex and risk
    prep_adults_sex_risk <- (df %>%
                               filter(Age >= 15 & Age <= 54) %>%
                               group_by(Gender, risk_group, Year, run_num) %>%
                               summarise(prep_cov = 100 * sum(HasIntervention.PrEP.)/(sum(Population) - sum(Infected)))
    )
    
    ## Person-time on PrEP by age, sex, risk, and year
    ## Note that this is approximate. It will be most accurate when the report_interval is set to the model time step (one month). I suspect it will overestimate PrEP person-time when the report_interval is substantially longer than the model time step. This error will increase when PrEP duration is shorter.
    prep_persontime_age_sex_risk_year <- (df %>%
                                            group_by(age_group, Gender, risk_group, year_floor, run_num)) %>%
      summarise(prep_py = sum(HasIntervention.PrEP.*report_interval +
                          PrEP_Expired*report_interval/2 +
                          PrEP_Aborted*report_interval/2)) %>%
      mutate(Year = year_floor)
    
    ## Total number of new infections, by age, sex, and risk, and year
    new_infections_age_sex_risk_year <- (df %>%
                                           group_by(age_group, Gender, risk_group, year_floor, run_num)) %>%
      summarise(new_infections = sum(Newly.Infected)) %>%
      mutate(Year = year_floor)
    
    return(list("pop" = pop, "pop_age_sex_risk" = pop_age_sex_risk, "pop_adults_sex_risk" = pop_adults_sex_risk,"prev_adults_sex" = prev_adults_sex, "prev_age_sex" = prev_age_sex, "prev_age_sex_risk" = prev_age_sex_risk, "prev_adults_sex_risk" = prev_adults_sex_risk, "inc_age_sex" = inc_age_sex, "inc_age_sex_risk" = inc_age_sex_risk, "inc_adults_sex_risk" = inc_adults_sex_risk, "inc_adults_sex" = inc_adults_sex, "art_sex" = art_sex, "art_adults_sex_risk" = art_adults_sex_risk, "prep_age_sex" = prep_age_sex, "prep_age_sex_risk" = prep_age_sex_risk, "prep_adults_sex_risk" = prep_adults_sex_risk, "prep_persontime_age_sex_risk_year" = prep_persontime_age_sex_risk_year, "new_infections_age_sex_risk_year" = new_infections_age_sex_risk_year))
    
  }
  
  ## Save output
  save(outputs, file = file.path(file_path, "output.RData"))

  if(make_plots == TRUE) {
    ## Plots
    pdf(file = file.path(file_path, "plots.pdf"), height = 5, width = 7)
    
    ## Total population over time
    print(ggplot(data = outputs$pop, aes(x = Year, y = pop)) +
            geom_line(aes(group = run_num), alpha = 0.8) +
            labs(x = "Year", y = "Population size") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("Total population") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    
    ## Population size by age, sex, and risk group
    print(ggplot(data = outputs$pop_age_sex_risk, aes(x = Year, y = pop, color = Gender, linetype = risk_group)) +
            geom_line(aes(group = interaction(run_num, Gender, risk_group)), alpha = 0.8) +
            labs(x = "Year", y = "Population size") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) + 
            ggtitle("Total population by age and sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    
    ## Population size, adults by sex and risk,
    print(ggplot(data = outputs$pop_adults_sex_risk, aes(x = Year, y = pop, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Population size") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE) + 
            ggtitle("Total population, ages 15-54, by sex and risk, same scale") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    print(ggplot(data = outputs$pop_adults_sex_risk, aes(x = Year, y = pop, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Population size") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE, scales = "free_y") + 
            ggtitle("Total population, ages 15-54, by sex and risk, free scale") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    
    ## Prevalence - age/sex
    print(ggplot(data = outputs$prev_age_sex, aes(x = Year, y = prev, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Prevalence (%)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) + 
            ggtitle("HIV prevalence by age and sex") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## Prevalence - age/sex/risk
    print(ggplot(data = outputs$prev_age_sex_risk, aes(x = Year, y = prev, color = Gender, linetype = risk_group)) +
            geom_line(aes(group = interaction(run_num, Gender, risk_group)), alpha = 0.8) +
            labs(x = "Year", y = "Prevalence (%)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) + 
            ggtitle("HIV prevalence by age and sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## Prevalence - adults by sex and risk
    print(ggplot(data = outputs$prev_adults_sex_risk, aes(x = Year, y = prev, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Prevalence (%)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE) + 
            ggtitle("HIV prevalence, ages 15-54, by sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    
    ## Incidence - age/sex
    print(ggplot(data = outputs$inc_age_sex, aes(x = Year, y = inc, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) +
            ggtitle("HIV incidence by age and sex") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## Incidence - age/sex/risk
    print(ggplot(data = outputs$inc_age_sex_risk, aes(x = Year, y = inc, color = Gender, linetype = risk_group)) +
            geom_line(aes(group = interaction(run_num, Gender, risk_group)), alpha = 0.8) +
            labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) +
            ggtitle("HIV incidence by age and sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## Incidence - adults by sex and risk
    print(ggplot(data = outputs$inc_adults_sex_risk, aes(x = Year, y = inc, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE) +
            ggtitle("HIV incidence, ages 15-54 by sex and risk, same scale") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    print(ggplot(data = outputs$inc_adults_sex_risk, aes(x = Year, y = inc, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "Incidence Rate (per 100 PY)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE, scales = "free_y") +
            ggtitle("HIV incidence, ages 15-54 by sex and risk, free scale") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## ART coverage - sex
    print(ggplot(data = outputs$art_sex, aes(x = Year, y = art_cov, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "ART Coverage (%)") +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("ART coverage by sex") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## ART coverage, adults by sex and risk group
    print(ggplot(data = outputs$art_adults_sex_risk, aes(x = Year, y = art_cov, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "ART Coverage (%)") +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE) +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("ART coverage, ages 15-54 by sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    
    ## PrEP coverage - age + sex
    print(ggplot(data = outputs$prep_age_sex, aes(x = Year, y = prep_cov, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "PrEP Coverage (%)") +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("PrEP coverage by age and sex") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## PrEP coverage - age + sex + risk
    print(ggplot(data = outputs$prep_age_sex_risk, aes(x = Year, y = prep_cov, color = Gender, linetype = risk_group)) +
            geom_line(aes(group = interaction(run_num, Gender, risk_group)), alpha = 0.8) +
            labs(x = "Year", y = "PrEP Coverage (%)") +
            facet_rep_wrap(~age_group, repeat.tick.labels = TRUE) +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("PrEP coverage by age and sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    ## PrEP coverage - adults by sex + risk
    print(ggplot(data = outputs$prep_adults_sex_risk, aes(x = Year, y = prep_cov, color = Gender)) +
            geom_line(aes(group = interaction(run_num, Gender)), alpha = 0.8) +
            labs(x = "Year", y = "PrEP Coverage (%)") +
            facet_rep_wrap(~risk_group, repeat.tick.labels = TRUE) +
            scale_x_continuous(limits = c(1980, 2060), breaks = seq(1980, 2060, by = 20)) +
            ggtitle("PrEP coverage, ages 15-54, by sex and risk") + 
            guides(colour = guide_legend(override.aes = list(alpha = 1))))
    
    dev.off()
  }  
  
  
}
