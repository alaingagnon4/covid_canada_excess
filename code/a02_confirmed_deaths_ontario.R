source("code/00_functions.R")

# case and death data
url <- "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv"
# download.file(url, destfile = "data_input/confirmed_ontario.csv")

# loading data
dts <- read_csv('data_input/confirmed_ontario.csv')

unique(dts$Outcome1)
unique(dts$Client_Gender)

dts %>% 
  filter(Outcome1 == "Fatal") %>% 
  summarise(tot = n())

#------- If the episode date is before Jan 21, 2020 - changing to case reported date
dts2 <- 
  dts %>%
  mutate(date = if_else(Accurate_Episode_Date < ymd("2020-01-22"), 
                      Case_Reported_Date, 
                      Accurate_Episode_Date),
         new = ifelse(Outcome1 == "Fatal", 1, 0),
         age = str_sub(Age_Group, 1, 2),
         age = case_when(age == "<2" ~ "0", 
                         age == "UN" ~ "unk",
                         TRUE ~ age),
         sex = case_when(Client_Gender == "FEMALE" ~ "f",
                         Client_Gender == "MALE" ~ "m",
                         TRUE ~ "unk")) %>% 
  select(date, sex, age, new) %>% 
  group_by(date, sex, age) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  complete(date, sex, age, fill = list(new = 0))

dts2 %>% 
  summarise(tot = sum(new))


dts_all_sex <- 
  dts2 %>% 
  group_by(date, age) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(sex = "t")

dts_all_age <- 
  dts2 %>% 
  group_by(date, sex) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(age = "All")

dts_all_age_sex <- 
  dts2 %>% 
  group_by(date) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(sex = "t",
         age = "All")

dts_all_age_sex %>% 
  summarise(tot = sum(new))

dts3 <- 
  dts2 %>% 
  bind_rows(dts_all_sex, dts_all_age, dts_all_age_sex) %>% 
  filter(age != "unk", 
         sex != "unk") %>% 
  ungroup() %>% 
  arrange(sex, age, date) %>% 
  complete(date, sex, age, fill = list(new = 0)) %>% 
  mutate(week = epiweek(date),
         year = epiyear(date)) %>% 
  group_by(sex, age, year, week) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  arrange(sex, age, year, week) %>% 
  left_join(weeks_dates) %>% 
  select(date, sex, age, new) %>% 
  filter(date >= "2020-03-01")

dts4 <- 
  dts3 %>% 
  group_by(sex, age) %>% 
  mutate(dts = cumsum(new),
         source = "by_age_sex") %>% 
  ungroup()

# write_rds(dts4, "data_inter/confirmed_deaths_ontario.rds")



# harmonizing Ontario ages to those of StatCan ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts5 <- 
  dts4 %>% 
  filter(age != "All") %>% 
  group_by(date, sex) %>% 
  do(harmonize_age_statcan(db = .data)) %>% 
  ungroup()


# comparison between data by age and all deaths by week ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data for all ages
daily_all <- 
  read_csv('data_input/ontario_All case trends data.csv') %>% 
  select(date = 1,
         place = 2,
         new = 13) %>% 
  mutate(date = mdy(date)) %>% 
  filter(place == "Ontario", 
         date >= "2020-03-01") %>% 
  select(-place) %>% 
  complete(date = seq(ymd(min(date)),ymd(max(date)), by = '1 day'),
           fill = list(new = 0)) %>% 
  mutate(week = epiweek(date),
         year = epiyear(date)) %>% 
  group_by(year, week) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  left_join(weeks_dates) %>% 
  select(-year, -week) %>% 
  mutate(dts = cumsum(new),
         source = "totals") %>% 
  ungroup() %>% 
  select(date, new, dts, source)

daily_all %>% 
  ggplot()+
  geom_point(aes(date, new))

daily_all %>% 
  ggplot()+
  geom_point(aes(date, dts))

daily_all %>% 
  summarise(tot = sum(new))


# both data
dts_both <- 
  dts4 %>% 
  filter(sex == "t", age == "All") %>% 
  select(date, new) %>% 
  mutate(source = "by_age_sex") %>% 
  bind_rows(daily_all %>% 
              select(date, new, source))

dts_both %>% 
  ggplot()+
  geom_line(aes(date, new, col = source))+
  theme_bw()

# how long is the lag? 14 days?
dts_both %>% 
  mutate(date = ifelse(source == "by_age_sex", date + days(14), date)) %>% 
  ggplot()+
  geom_line(aes(date, new, col = source))+
  theme_bw()


# Adjustment of lag ====
# ~~~~~~~~~~~~~~~~~~~~~~
# imputation of age structure
age_dist <- 
  dts5 %>% 
  filter(age != "All", 
         sex != "t") %>% 
  mutate(date = date + days(14)) %>% 
  group_by(date) %>% 
  mutate(prop = ifelse(dts != 0, dts / sum(dts), 0)) %>% 
  ungroup() %>% 
  left_join(daily_all %>% 
              rename(dts_all = dts) %>% 
              select(date, dts_all)) %>% 
  mutate(dts_adj = round(dts_all * prop))

age_dist2 <- 
  age_dist %>% 
  select(date, sex, age, dts = dts_adj) %>% 
  drop_na(dts) %>% 
  group_by(sex, age) %>% 
  mutate(new = dts - lag(dts)) %>% 
  ungroup() %>% 
  mutate(new = ifelse(is.na(new) | new <= 0, 0, new)) %>% 
  group_by(sex, age) %>% 
  mutate(dts = cumsum(new))

age_dist3 <- 
  age_dist2 %>% 
  group_by(date, age) %>% 
  summarise(dts = sum(dts),
            new = sum(new)) %>% 
  ungroup() %>% 
  mutate(sex = "t") %>% 
  bind_rows(age_dist2)

write_rds(age_dist3, "data_inter/confirmed_deaths_ontario.rds")


# both data
dts_both <- 
  age_dist3 %>% 
  filter(sex == "t") %>% 
  group_by(date) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(source = "by_age_sex") %>% 
  bind_rows(daily_all %>% 
              select(date, new, source))

dts_both %>% 
  ggplot()+
  geom_line(aes(date, new, col = source))+
  theme_bw()


age_dist3 %>% 
  ggplot()+
  geom_line(aes(date, new, col = age, group = age))+
  facet_wrap(~sex)
  # filter(sex == "t") %>% 
  # group_by(date)
