source("code/00_functions.R")

qc <- 
  read_rds("weekly_deaths_exposures_quebec_ages_statcan.rds")

ca <- 
  read_rds("weekly_deaths_exposures_canada.rds")

out <- 
  ca %>% 
  filter(region != "Quebec") %>% 
  bind_rows(qc %>% select(-year, -week))

write_rds(out, "weekly_deaths_exposures_canada_statcan_isq.rds")
