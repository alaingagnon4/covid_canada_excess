source("code/00_functions.R")

# loading mortality data from Quebec files
# https://statistique.quebec.ca/fr/document/nombre-hebdomadaire-de-deces-au-quebec/tableau/deces-par-semaine-selon-le-groupe-dage-quebec#tri_gp=781
dts <- read_xlsx("data_input/DecesSemaine_QC_GrAge.xlsx",
                 skip = 5)

pop <- 
  read_rds("data_inter/weekly_exposures_quebec_age.rds")

# mortality data
# ~~~~~~~~~~~~~~~~~
dts2 <- 
  dts %>% 
  select(year = 1,
         age = 3,
         everything(),
         -Statut) %>% 
  drop_na(year) %>% 
  gather(-year, -age, key = week, value = dts) %>% 
  mutate(age = str_sub(age, 1, 2),
         age = case_when(age == "0-" ~ "0",
                         age == "To" ~ "all",
                         TRUE ~ age),
         dts = as.integer(dts)
  ) %>% 
  group_by(age, year) %>% 
  mutate(week = 1:n(),
         date = MMWRweek2Date(MMWRyear=year,MMWRweek=week,MMWRday=7)) %>% 
  ungroup() %>% 
  drop_na(dts)

unique(dts2$age)

dts2 %>% 
  select(year, week) %>% 
  group_by(year) %>% 
  summarise(weeks = max(week))

pop %>% 
  select(year, week) %>% 
  group_by(year) %>% 
  summarise(weeks = max(week))

# r <- c("Quebec", "British Columbia", "Alberta")
a <- "all"

dts2 %>% 
  filter(age == a) %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()

# adding exposures
dts3 <- 
  dts2 %>% 
  left_join(pop %>% filter(sex == "t"))

write_rds(dts3, "weekly_deaths_exposures_quebec.rds")








source("code/00_functions.R")

# loading mortality data from Quebec files
# https://statistique.quebec.ca/fr/document/nombre-hebdomadaire-de-deces-au-quebec/tableau/deces-par-semaine-selon-le-groupe-dage-quebec#tri_gp=781
dts <- read_xlsx("data_input/2022-04-20 Décès hebdos GrAgeSexe 2010-2021_Pour Acosta.xlsx")

pop <- 
  read_rds("data_inter/weekly_exposures_canada_age.rds")

# mortality data
# ~~~~~~~~~~~~~~~~~
dts2 <- 
  dts %>% 
  select(sex = 1,
         age = 5,
         year = 2,
         week = 4,
         dts = 6,
         -Statut) %>% 
  drop_na(year) %>% 
  mutate(age = str_sub(age, 1, 2),
         age = ifelse(age == "0_", 0, age), 
         age = as.double(age),
         age = case_when(age < 45 ~ 0,
                         age %in% 45:64 ~ 45,
                         age %in% 65:84 ~ 65,
                         age >= 85 ~ 85),
         sex = recode(sex,
                      "1" = "m",
                      "2" = "f")) %>% 
  group_by(year, week, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(date = MMWRweek2Date(MMWRyear=year,MMWRweek=week,MMWRday=7),
         age = age %>% as.character())

dts_all_age <- 
  dts2 %>% 
  group_by(year, week, date, sex) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(age = "all")

dts_all_sex <- 
  dts2 %>% 
  group_by(year, week, date, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(sex = "t")

dts_all_sex_age <- 
  dts2 %>% 
  group_by(year, week, date) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(sex = "t",
         age = "all")

dts3 <- 
  bind_rows(dts2,
            dts_all_age,
            dts_all_sex,
            dts_all_sex_age)
            
unique(dts3$age)
unique(dts3$sex)

dts3 %>% 
  select(year, week) %>% 
  group_by(year) %>% 
  summarise(weeks = max(week))

# pop %>% 
#   select(year, week) %>% 
#   group_by(year) %>% 
#   summarise(weeks = max(week))

# r <- c("Quebec", "British Columbia", "Alberta")
a <- "all"
s <- "t"

dts3 %>% 
  filter(age == a,
         sex == s) %>% 
  ggplot()+
  geom_line(aes(date, dts))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()

# adding exposures
dts4 <- 
  dts3 %>% 
  left_join(pop %>% filter(region == "Quebec"))

write_rds(dts4, "weekly_deaths_exposures_quebec_ages_statcan.rds")





