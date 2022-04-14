source("code/00_functions.R")

# loading mortality data from Quebec files
# https://statistique.quebec.ca/fr/document/nombre-hebdomadaire-de-deces-au-quebec/tableau/deces-par-semaine-selon-le-groupe-dage-quebec#tri_gp=781
dts <- read_xlsx("data_input/DecesSemaine_QC_GrAge.xlsx",
                 skip = 5)

pop <- read_rds("data_inter/weekly_exposures_quebec_age.rds")

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
         # sex = case_when(sex == "Both sexes" ~ "t",
         #                 sex == "Males" ~ "m",
         #                 sex == "Females" ~ "f"),
         dts = as.integer(dts)
  ) %>% 
  group_by(age, year) %>% 
  mutate(week = 1:n(),
         isoweek = paste0(year, "-W", sprintf('%02d', week), "-7"),
         date = ISOweek2date(isoweek)) %>% 
  ungroup() %>% 
  drop_na(dts)

unique(dts2$age)

dts2 %>% 
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
