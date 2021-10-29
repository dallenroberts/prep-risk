################################################################################
## Allen Roberts
## PrEP campaign comparison
################################################################################

## Libraries
library(tidyverse)
library(purrr)
library(ggplot2)
library(viridis)
library(viridisLite)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(broom)

theme_set(theme_classic())

## Function to combine results across different runs
comb <- function(x, ...) {  
  mapply(rbind,x,...,SIMPLIFY=FALSE)
}

## Run information
risk_group_suite_name <- "prep_risk_group_sweep_first25"
partnership_suite_name <- "prep_partnership_sweep_first25"
scale_factor <- 0.05

## Load risk group results
load(file.path("output", risk_group_suite_name, "py_on_prep.RData"))
load(file.path("output", risk_group_suite_name, "report_hiv_results.RData"))

report_hiv_results_risk_group <- report_hiv_results
py_on_prep_results_risk_group <- py_on_prep_results

## Combine with relationship results
load(file.path("output", partnership_suite_name, "py_on_prep.RData"))
load(file.path("output", partnership_suite_name, "report_hiv_results.RData"))

report_hiv_results <- comb(report_hiv_results, report_hiv_results_risk_group)
py_on_prep_results <- comb(py_on_prep_results, py_on_prep_results_risk_group)

## Function to extract campaign details from campaign_name
get_campaign_details <- function(campaign_name, return = "type") {
  
  library(tidyverse)
  
  ## Baseline campaign
  if(str_count(campaign_name, pattern = "Baseline") == 2) {
    
    campaign_type <- "Baseline"
    campaign_subtype <- "Baseline"
    
  } else if(grepl("partnership", campaign_name)) {
    
    campaign_type <- "Partnership"
    
    campaign_subtype <- paste("RR", last(str_split(campaign_name, "-")[[1]]), sep = "-")
    
  } else if(grepl("risk_group", campaign_name)) {
    
    campaign_type <- "Risk_group"
    
    low_risk_cov <- as.integer(nth(str_split(campaign_name, "-")[[1]], n = -3))
    med_risk_cov <- as.integer(nth(str_split(campaign_name, "-")[[1]], n = -2))
    high_risk_cov <- as.integer(nth(str_split(campaign_name, "-")[[1]], n = -1))
    
    if(low_risk_cov == 0 & med_risk_cov == 0) {
      
      campaign_subtype <- "High"
      
    } else if(low_risk_cov == 0 & med_risk_cov > 0) {
      
      campaign_subtype <- "Medium+High"
      
    } else {
      
      campaign_subtype  <- "Low+Medium+High"
      
    }
  }
  
  if(return == "type") {
    
    return(campaign_type)
    
  } else {
    
    return(campaign_subtype)
  }

}
           
## Format campaign names
## Think about moving this out as well
for(i in 1:length(report_hiv_results)) {
  
  report_hiv_results[[i]] <- report_hiv_results[[i]] %>%
    mutate(campaign_type = map_chr(campaign, get_campaign_details, return = "type"),
           campaign_subtype = map_chr(campaign, get_campaign_details, return = "subtype"))
  
}

for(i in 1:length(py_on_prep_results)) {
  
  py_on_prep_results[[i]] <- py_on_prep_results[[i]] %>%
    mutate(campaign_type = map_chr(campaign, get_campaign_details, return = "type"),
           campaign_subtype = map_chr(campaign, get_campaign_details, return = "subtype"))
  
}

## Need to make these by run in order to get uncertainty
for(i in 1:length(report_hiv_results)) {
  
  ## Infections averted, relative to baseline
  if(grepl("num_infections", names(report_hiv_results)[i])) {
    
    report_hiv_results[[i]] <- report_hiv_results[[i]] %>%
      mutate(infections_averted = median[campaign_type == "Baseline"] - median,
             prop_infections_averted = (median[campaign_type == "Baseline"] - median)/median[campaign_type == "Baseline"]) 
  }
  
  ## Relative risk, compared to baseline
  if(grepl("inc_", names(report_hiv_results)[i])) {
    
    report_hiv_results[[i]] <- report_hiv_results[[i]] %>%
      mutate(relative_risk = median/ median[campaign_type == "Baseline"])
  }
  
}

## Person-time on PrEP by type of PrEP
p0 <- py_on_prep_results$py_on_prep_by_type %>%
  filter(campaign_type == "Partnership") %>%
  group_by(campaign, campaign_subtype, PrEP_type) %>%
  summarise(py_on_prep = sum(median)) %>%
  group_by(campaign_subtype, PrEP_type) %>%
  summarise(py_on_prep = median(py_on_prep)) %>%
  mutate(prop_py = py_on_prep/sum(py_on_prep),
         PrEP_type = factor(PrEP_type, levels = c("PrEP_Neg_Received", "PrEP_Pos_Received", "PrEP_Partner_Received", "PrEP_High_Received"),
                            labels = c("Low+Med Risk, Partner Negative", "Low+Med Risk, New Partner Positive", "Low+Med Risk, Partner Newly Infected", "High Risk"))) %>%
  ggplot(aes(x = campaign_subtype, y = prop_py, group = PrEP_type)) +
    geom_bar(aes(fill = PrEP_type), position = "dodge", stat = "identity") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = "PrEP Scenario", y = "Proportion of PrEP person-time") +
    scale_fill_brewer(palette = "Spectral", name = "PrEP condition") +
    guides(fill = guide_legend(ncol = 2)) +
    theme(legend.position = "bottom")

## Same plot, but by gender
p0_gender <- py_on_prep_results$py_on_prep_by_type %>%
  filter(campaign_type == "Partnership") %>%
  group_by(gender, campaign_subtype, PrEP_type) %>%
  summarise(py_on_prep = median(median)) %>%
  mutate(prop_py = py_on_prep/sum(py_on_prep),
         PrEP_type = factor(PrEP_type, levels = c("PrEP_Neg_Received", "PrEP_Pos_Received", "PrEP_Partner_Received", "PrEP_High_Received"),
                            labels = c("Low+Med Risk, Partner Negative", "Low+Med Risk, New Partner Positive", "Low+Med Risk, Partner Newly Infected", "High Risk"))) %>%
  ggplot(aes(x = campaign_subtype, y = prop_py, group = PrEP_type)) +
  geom_bar(aes(fill = PrEP_type), position = "dodge", stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = "PrEP Scenario", y = "Proportion of PrEP person-time") +
  scale_fill_brewer(palette = "Spectral", name = "PrEP condition") +
  facet_wrap(~gender) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom")


## Total person-time on PrEP
## Might want to move this out as well to calculate uncertainty
py_on_prep <- py_on_prep_results$py_on_prep %>%
  group_by(campaign, campaign_type, campaign_subtype) %>%
  summarise(py_on_prep = sum(median))

## Plotting color schemes
risk_group_colors <- brewer.pal(11, "Spectral")[c(1,2,3)]
partnership_colors <- brewer.pal(11, "Spectral")[c(9,10,11)]

all_colors <- c(risk_group_colors, partnership_colors)

subtype_names <- c("High", "Medium+High", "Low+Medium+High", "RR-1", "RR-2", "RR-3")
names(all_colors) <-  subtype_names

all_shapes <- c(rep(17, 3), rep(19, 3))
names(all_shapes) <- subtype_names

all_linetypes <- c(rep("dotted", 3), rep("solid", 3))
names(all_linetypes) <- subtype_names

## Infections averted vs. person-time on PrEP
p1_data <- report_hiv_results$num_infections %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(infections_averted, campaign) %>%
  left_join(py_on_prep, by = "campaign") %>%
  mutate(py_on_prep = py_on_prep/1000,
         infections_averted = infections_averted/1000,
         campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p1_full <- p1_data %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
    geom_point() + 
    geom_line() +
    scale_linetype_manual(values = all_linetypes) +
    scale_shape_manual(values = all_shapes) +
    scale_color_manual(values = all_colors) +
    labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
    theme(legend.position = "none")

p1_rg <- p1_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
    geom_point() +
    geom_line() +
    scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
    scale_shape_manual(values = all_shapes, name = "Risk Group") +
    scale_color_manual(values = all_colors, name = "Risk Group") +
    labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
    theme(legend.justification = "top")

p1_part <- p1_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
  theme(legend.justification = "bottom")
  
p1 <- plot_grid(p1_full,
          plot_grid(
            get_legend(p1_part),
            get_legend(p1_rg),
            align = "v",
            ncol = 1
          ),
          nrow = 1,
          rel_widths = c(1, 0.5)
          )

## NNT calculation: Person-years of PrEP per infection averted
report_hiv_results$num_infections %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(infections_averted, campaign) %>%
  left_join(py_on_prep, by = "campaign") %>%
  group_by(campaign_subtype) %>%
  do(tidy(lm(py_on_prep ~ infections_averted, .))) %>%
  filter(term == "infections_averted")

## Same plot, but by gender
p1_gender_data <- report_hiv_results$num_infections_gender %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(infections_averted, gender, campaign) %>%
  left_join(py_on_prep_results$py_on_prep, by = c("gender", "campaign")) %>%
  rename("py_on_prep" = "median") %>%
  mutate(py_on_prep = py_on_prep/1000,
         infections_averted = infections_averted/1000,
         campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p1_gender_full <- p1_gender_data %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = all_linetypes) +
  scale_shape_manual(values = all_shapes) +
  scale_color_manual(values = all_colors) +
  labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
  theme(legend.position = "none") +
  facet_wrap(~gender)

p1_gender_rg <- p1_gender_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
  scale_shape_manual(values = all_shapes, name = "Risk Group") +
  scale_color_manual(values = all_colors, name = "Risk Group") +
  labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
  theme(legend.justification = "top") +
  facet_wrap(~gender)

p1_gender_part <- p1_gender_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = py_on_prep, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  labs(x = "Person-years on PrEP\n(thousands)", y = "Infections averted\n(thousands)") +
  theme(legend.justification = "bottom") +
  facet_wrap(~gender)

p1_gender <- plot_grid(p1_gender_full,
                plot_grid(
                  get_legend(p1_gender_part),
                  get_legend(p1_gender_rg),
                  align = "v",
                  ncol = 1
                ),
                nrow = 1,
                rel_widths = c(1, 0.25)
)

## NNT calculation: Person-years of PrEP per infection averted
report_hiv_results$num_infections_gender %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(infections_averted, gender, campaign) %>%
  left_join(py_on_prep_results$py_on_prep, by = c("gender", "campaign")) %>%
  rename("py_on_prep" = "median") %>%
  group_by(gender, campaign_subtype) %>%
  do(tidy(lm(py_on_prep ~ infections_averted, .))) %>%
  filter(term == "infections_averted")
  
## PrEP coverage vs. number of infections averted
p2_data <- report_hiv_results$num_infections %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(infections_averted, campaign) %>%
  left_join(report_hiv_results$prep_cov, by = "campaign") %>%
  rename("prep_cov" = "median") %>%
  select(campaign, campaign_type, campaign_subtype, infections_averted, prep_cov) %>%
  mutate(infections_averted = infections_averted/1000,
         campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p2_full <- p2_data %>%
  ggplot(aes(x = prep_cov, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = all_linetypes) +
  scale_shape_manual(values = all_shapes) +
  scale_color_manual(values = all_colors) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Infections averted\n(thousands)") +
  theme(legend.position = "none")

p2_rg <- p2_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = prep_cov, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
  scale_shape_manual(values = all_shapes, name = "Risk Group") +
  scale_color_manual(values = all_colors, name = "Risk Group") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Infections averted\n(thousands)") +
  theme(legend.justification = "top")

p2_part <- p2_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = prep_cov, y = infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Infections averted\n(thousands)") +
  theme(legend.justification = "bottom")

p2 <- plot_grid(p2_full,
                plot_grid(
                  get_legend(p2_part),
                  get_legend(p2_rg),
                  align = "v",
                  ncol = 1
                ),
                nrow = 1,
                rel_widths = c(1, 0.5)
)

## Proportion of infections among 15-34 year olds averted vs. PrEP coverage
p3_data <- report_hiv_results$num_infections %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(prop_infections_averted, campaign) %>%
  left_join(report_hiv_results$prep_cov, by = "campaign") %>%
  rename("prep_cov" = "median") %>%
  select(campaign, campaign_type, campaign_subtype, prop_infections_averted, prep_cov) %>%
  mutate(campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p3_full <- p3_data %>%
  ggplot(aes(x = prep_cov, y = prop_infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = all_linetypes) +
  scale_shape_manual(values = all_shapes) +
  scale_color_manual(values = all_colors) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "% of infections among 15-34 year olds averted") +
  theme(legend.position = "none")

p3_rg <- p3_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = prep_cov, y = prop_infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
  scale_shape_manual(values = all_shapes, name = "Risk Group") +
  scale_color_manual(values = all_colors, name = "Risk Group") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "% of infections among 15-34 year olds averted") +
  theme(legend.justification = "top")

p3_part <- p3_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = prep_cov, y = prop_infections_averted, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "% of infections among 15-34 year olds averted") +
  theme(legend.justification = "bottom")

p3 <- plot_grid(p3_full,
                plot_grid(
                  get_legend(p3_part),
                  get_legend(p3_rg),
                  align = "v",
                  ncol = 1
                ),
                nrow = 1,
                rel_widths = c(1, 0.5)
)



## PrEP coverage vs. relative risk
p4_data <- report_hiv_results$inc_15_34 %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(relative_risk, campaign) %>%
  left_join(report_hiv_results$prep_cov, by = "campaign") %>%
  rename("prep_cov" = "median") %>%
  select(campaign, campaign_type, campaign_subtype, relative_risk, prep_cov) %>%
  mutate(campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p4_full <- p4_data %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = all_linetypes) +
  scale_shape_manual(values = all_shapes) +
  scale_color_manual(values = all_colors) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Relative risk") +
  theme(legend.position = "none")

p4_rg <- p4_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
  scale_shape_manual(values = all_shapes, name = "Risk Group") +
  scale_color_manual(values = all_colors, name = "Risk Group") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Relative risk") +
  theme(legend.justification = "bottom")

p4_part <- p4_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Relative risk") +
  theme(legend.justification = "top")

p4 <- plot_grid(p4_full,
                plot_grid(
                  get_legend(p4_rg),
                  get_legend(p4_part),
                  align = "v",
                  ncol = 1
                ),
                nrow = 1,
                rel_widths = c(1, 0.5)
)

## Same plot, by gender
p4_gender_data <- report_hiv_results$inc_15_34_gender %>% 
  ungroup() %>%
  filter(campaign_type != "Baseline") %>%
  select(gender, relative_risk, campaign) %>%
  left_join(report_hiv_results$prep_cov_gender, by = c("gender", "campaign")) %>%
  rename("prep_cov" = "median") %>%
  select(gender, campaign, campaign_type, campaign_subtype, relative_risk, prep_cov) %>%
  mutate(campaign_subtype = factor(campaign_subtype, levels = subtype_names))

p4_gender_full <- p4_gender_data %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = all_linetypes) +
  scale_shape_manual(values = all_shapes) +
  scale_color_manual(values = all_colors) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Relative risk") +
  theme(legend.position = "none") +
  facet_wrap(~gender)

p4_gender_rg <- p4_gender_data %>%
  filter(campaign_type == "Risk_group") %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Risk Group") +
  scale_shape_manual(values = all_shapes, name = "Risk Group") +
  scale_color_manual(values = all_colors, name = "Risk Group") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "% of infections among 15-34 year olds averted") +
  theme(legend.justification = "bottom") +
  facet_wrap(~gender)

p4_gender_part <- p4_gender_data %>%
  filter(campaign_type == "Partnership") %>%
  ggplot(aes(x = prep_cov, y = relative_risk, linetype = campaign_subtype, shape = campaign_subtype, color = campaign_subtype)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = all_linetypes, name = "Partnership") +
  scale_shape_manual(values = all_shapes, name = "Partnership") +
  scale_color_manual(values = all_colors, name = "Partnership") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "% HIV-negative 15-34 on PrEP", y = "Relative risk") +
  theme(legend.justification = "top") + 
  facet_wrap(~gender)

p4_gender <- plot_grid(p4_gender_full,
                plot_grid(
                  get_legend(p4_gender_rg),
                  get_legend(p4_gender_part),
                  align = "v",
                  ncol = 1
                ),
                nrow = 1,
                rel_widths = c(1, 0.25)
)

## Save plots
pdf(file = file.path("figures", "prep_campaign_results.pdf"), height = 6, width = 8)
print(p0)
print(p0_gender)
print(p1)
print(p1_gender)
print(p2)
print(p3)
print(p4)
print(p4_gender)
dev.off()

## Individual plots
