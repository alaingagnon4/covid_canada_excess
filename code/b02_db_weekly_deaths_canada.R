source("code/00_functions.R")

# loading mortality data from StatCan files
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310076801
dts <- read_csv("data_input/13100768-eng.zip",
                   col_types = cols(.default = "c"))

pop <- read_rds("data_inter/weekly_exposures_province_age.rds")

# mortality data
# ~~~~~~~~~~~~~~~~~
dts2 <- 
  dts %>% 
  rename(region = GEO,
         date = REF_DATE,
         age = 'Age at time of death',
         sex = Sex,
         dts = VALUE) %>% 
  select(region, date, age, sex, dts) %>% 
  mutate(region = str_remove(region, ", place of occurrence"),
         date = ymd(date),
         age = str_remove(age, "Age at time of death, "),
         age = str_trim(str_sub(age, 1, 2)),
         age = ifelse(age == "al", "All", age),
         sex = case_when(sex == "Both sexes" ~ "t",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         dts = as.integer(dts),
         year = epiyear(date)
         # isoweek = date2ISOweek(date),
         # isoweek = paste0(str_sub(isoweek, 1, 9), "7")
         ) %>% 
  group_by(region, sex, age, year) %>% 
  mutate(week = 1:n(),
         isoweek = paste0(year, "-W", sprintf('%02d', week), "-7"))

unique(dts2$age)
unique(dts2$region)
table(dts2$region)

dts2 %>% 
  select(year, week) %>% 
  group_by(year) %>% 
  summarise(weeks = max(week))


r <- c("Quebec", "British Columbia", "Alberta")
a <- "All"

dts2 %>% 
  filter(region %in% r,
         age == a,
         sex == "t") %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  facet_wrap(~region, scales = "free")

# adding exposures
dts3 <- 
  dts2 %>% 
  left_join(pop)

write_rds(dts3, "weekly_deaths_exposures.rds")
