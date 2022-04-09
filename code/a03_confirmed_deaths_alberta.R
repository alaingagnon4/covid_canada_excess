source("code/00_functions.R")

url <- "https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv"
# download.file(url, destfile = "data_input/confirmed_alberta.csv")
# loading data
dts <- 
  read_csv('data_input/confirmed_alberta.csv',
           col_types = cols(.default = "c"))

unique(dts$Gender)

dts2 <- 
  dts %>% 
  select(date = 2,
         sex = 4,
         age = 5,
         status = 6) %>% 
  filter(status == "Died") %>% 
  mutate(
    age = str_sub(age, 1, 2),
    age = case_when(age %in% c("1-", "5-") ~ "0",
                    age == "Un" ~ "unk",
                    TRUE ~ age),
    date = ymd(date),
    sex = case_when(sex == "Male" ~ "m",
                    sex == "Female" ~ "f",
                    TRUE ~ "unk")) %>% 
  select(-status) %>% 
  group_by(date, sex, age) %>% 
  summarise(new = n()) %>% 
  ungroup() %>% 
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

unique(dts2$age)
unique(dts2$date)
unique(dts2$sex)

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

dts3 <- 
  dts2 %>% 
  bind_rows(dts_all_sex, dts_all_age, dts_all_age_sex) %>% 
  filter(age != "unk", 
         sex != "unk") %>% 
  ungroup() %>% 
  complete(date, sex, age, fill = list(new = 0))

unique(dts3$age)
unique(dts3$sex)

dts4 <- 
  dts3 %>% 
  arrange(date, sex, age) %>% 
  group_by(age, sex) %>% 
  mutate(dts = cumsum(new),
         source = "by_age_sex") %>% 
  ungroup()

# saving data
# write_rds(dts4, "data_inter/confirmed_deaths_alberta.rds")


# harmonizing Ontario ages to those of StatCan ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts5 <- 
  dts4 %>% 
  filter(age != "All") %>% 
  group_by(date, sex) %>% 
  do(harmonize_age_statcan(db = .data)) %>% 
  ungroup()


daily_all <- 
  read_csv("data_input/covid-19-alberta-statistics-summary-data.csv",
           col_types = cols(.default = "c")) %>% 
  select(date = 2,
         new = 10) %>% 
  mutate(new = new %>% as.double(),
         date = ymd(date)) %>% 
  complete(date = seq(ymd(min(date)), ymd(max(date)), by = '1 day'),
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

# both data
dts_both <- 
  dts4 %>% 
  filter(sex == "t", age == "All") %>% 
  select(date, new, source) %>% 
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

write_rds(age_dist3, "data_inter/confirmed_deaths_alberta.rds")


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
#   group_by(date)




