# source(here("Automation/00_Functions_automation.R"))
library(tidyverse)
library(lubridate)
db_qc_d <- read_csv("data_input/graph_2-1.a_page_age_et_sexe_evol_des_décès.csv")

db_qc_d2 <- 
  db_qc_d %>% 
  rename(date = 1) %>% 
  mutate(date = ymd(date)) %>% 
  gather(-date, key = age, value = new) %>% 
  group_by(age) %>% 
  mutate(dts = cumsum(new)) %>% 
  arrange(date, age) %>% 
  ungroup() %>% 
  select(-new) %>% 
  mutate(age = str_sub(age, 1, 2),
         age = ifelse(age == "0-", "0", age),
         age = age %>% as.double())

# saving data
write_rds(db_qc_d2, "data_inter/confirmed_deaths_quebec.rds")
