## Allen Roberts
## Create scenarios.csv file programmatically
## Don't overwrite existing files
## Assumes that we are working out of R project in prep-risk directory

rm(list = ls())

scenarios <- expand.grid("Scenario" = "base_case",
            "Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female" = c(59, seq(6, 12, by = 1)),
            # "Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Male" = seq(42, 58, by = 4),
            "Campaign" = "campaign_orig.json")

scenarios$Scenario <- paste0(scenarios$Scenario,
                            "_maxsimrelfemales", scenarios$Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Female)
                           #  "_maxsimrelmales", scenarios$Society__KP_Defaults.COMMERCIAL.Concurrency_Parameters.HIGH.Max_Simultaneous_Relationships_Male)

if(file.exists("scenarios.csv")) {
  stop("Error: scenarios.csv already exists - move manually and rename to avoid overwriting existing file.")
} else {
  write.csv(scenarios, file = "scenarios.csv", row.names = FALSE)
}
