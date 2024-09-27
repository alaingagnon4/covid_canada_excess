source("code/00_functions.R")

dts_qc <- read_csv("data_input/graph_2-1.a_page_age_et_sexe_evol_des_décès.csv")

dts_qc2 <- 
  dts_qc %>% 
  rename(date = 1) %>% 
  mutate(date = ymd(date)) %>% 
  gather(-date, key = age, value = new) %>% 
  mutate(age = str_sub(age, 1, 2),
         age = ifelse(age == "0-", "0", age),
         age = age %>% as.double()) %>% 
  mutate(week = epiweek(date),
         year = epiyear(date)) %>% 
  group_by(age, year, week) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  left_join(weeks_dates) %>% 
  select(date, age, new) %>% 
  group_by(age) %>% 
  mutate(dts = cumsum(new),
         sex = "t") %>% 
  arrange(date, age) %>% 
  ungroup()

# harmonizing Quebec ages to those of StatCan ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# dts_age_all <- 
#   dts_qc2 %>% 
#   group_by(date, sex) %>% 
#   summarise(new = sum(new),
#             dts = sum(dts)) %>% 
#   ungroup() %>% 
#   mutate(age = "all")

dts_qc3 <- 
  dts_qc2 %>% 
  group_by(date, sex) %>% 
  do(harmonize_age_statcan_isq(db = .data)) %>% 
  ungroup()

dts_qc4 <- 
  dts_qc3 %>% 
  mutate(dts = round(dts)) %>% 
  arrange(date, age) %>% 
  group_by(age, format) %>% 
  mutate(new = dts - lag(dts),
         new = ifelse(is.na(new), 0, new)) %>% 
  ungroup()

dts_qc4 %>% 
  group_by(format) %>% 
  summarise(sum(new))

dts_qc2 %>% 
  summarise(sum(new))

# saving data
write_rds(dts_qc4, "data_inter/confirmed_deaths_quebec.rds")
