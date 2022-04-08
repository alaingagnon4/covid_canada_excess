source("code/00_functions.R")

# provisionally, only for the main provinces and national

# loading mortality data from StatCan files
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1310076801
dts <- read_csv("data_input/13100768-eng.zip",
                   col_types = cols(.default = "c"))


# loading population data from StatCan files
pop_prj <- read_csv("data_input/1710005701_databaseLoadingData.csv",
                col_types = cols(.default = "c"))

pop_est <- read_csv("data_input/1710000501_databaseLoadingData.csv",
                col_types = cols(.default = "c"))



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
         sex = case_when(sex == "Both sexes" ~ "b",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         dts = as.integer(dts))

unique(dts2$age)
unique(dts2$region)
table(dts2$region)

r <- "Alberta"
a <- "85"

dts2 %>% 
  filter(region == r,
         age == a,
         sex == "b") %>% 
  ggplot()+
  geom_line(aes(date, dts))




# weeks and dates
#################
weeks <- expand_grid(Year = seq(2010, 2021, 1), Week = seq(1, 52, 1)) %>% 
  bind_rows(tibble(Year = c(2014, 2020), Week = 53)) %>% 
  arrange(Year, Week)

# weeks dates before 2020
dates_w <- 
  dts2 %>% 
  filter(region == r,
         age == a,
         sex == "b",
         date < "2020-01-04") %>% 
  select(date) 

# week 1 2020
first_date_w_2020 <- ymd("2020-01-04")

dates_w2 <- dates_w %>% 
  bind_rows(tibble(date = first_date_w_2020 + 7 * seq(0, 52, 1)))

weeks2 <- bind_cols(dates_w2, weeks) %>% 
  mutate(t = 1:n())


# population data
# ~~~~~~~~~~~~~~~~~~~~~~
pop_prj2 <- 
  pop_prj

pop_est2 <- 
  pop_est %>% 
  cc

unique(pop_est$`Age group`)
unique(pop_prj$`Age group`)

pop_est2 <- 
  pop_est %>% 
  rename(region = GEO,
         year = REF_DATE,
         age = 'Age group',
         sex = Sex,
         exposure = VALUE) %>% 
  select(region, year, age, sex, exposure) %>% 
  mutate(age = str_sub(age, 1, 3),
         age = case_when(age == "0 t" ~ "0", 
                         age == "5 t" ~ "5", 
                         TRUE ~ age),
         sex = case_when(sex == "Both sexes" ~ "b",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         exposure = as.integer(exposure),
         year = as.integer(year),
         week = 26) 

pop_prj2 <- 
  pop_prj %>% 
  rename(region = GEO,
         year = REF_DATE,
         age = 'Age group',
         sex = Sex,
         exposure = VALUE) %>% 
  select(region, year, age, sex, exposure)

prj_all_ages <- 
  pop_prj2 %>% 
  filter(age == "All ages")

pop_prj3 <- 
  pop_prj2 %>% 
  # filter(age != "All ages") %>% 
  mutate(age = str_sub(age, 1, 3),
         age = case_when(age == "0 t" ~ "0", 
                         age == "5 t" ~ "5", 
                         TRUE ~ age),
         sex = case_when(sex == "Both sexes" ~ "b",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         exposure = as.integer(exposure) * 1000,
         year = as.integer(year),
         week = 26) 


%>% 
  left_join(weeks2) %>% 
  mutate(t = ifelse(Year == 2009, -26, t))

# population weekly interpolation 
ages <- unique(pop2$Age)
rgs <- unique(pop2$Region)
rgs <- c("British Columbia", "Alberta", "Canada", "Ontario", "Quebec", "Manitoba", "Saskatchewan")
# r <- "Canada"
# s <- "f"
# a <- "50"
# rgs <- "Canada"
inters_pop <- NULL
for(r in rgs){
  for(s in c("m", "f", "b")){
    for(a in ages){
      
      db_w_temp <- pop2 %>% 
        filter(Region == r,
               Sex == s,
               Age == a)
      
      db_w_temp2 <- weeks2 %>% 
        left_join(interpop(db_w_temp)) %>% 
        mutate(Region = r,
               Age = a,
               Sex = s)
      
      inters_pop <- inters_pop %>% 
        bind_rows(db_w_temp2)
      
    }
  }
}

unique(inters_pop$Region)
unique(pop2$Region)
table(inters_pop$Region)
# Visual test
#############

r <- "Saskatchewan"
a <- "50"
s <- "f"

orig <- pop2 %>%
  filter(Region == r,
         Age == a,
         Sex == s) %>%
  left_join(weeks2) %>%
  mutate(Source = "Original")

inter <- inters_pop %>%
  filter(Region == r,
         Age == a,
         Sex == s,
         Week != 27) %>%
  mutate(Source = "Interpolation",
         Age = as.character(Age))

ggplot()+
  geom_line(data = inter, aes(t, Exposure), col = "red", alpha = 0.6)+
  geom_point(data = orig, aes(t, Exposure), col = "black")

############################################

# population by age for Canada data
pop3 <- inters_pop %>% 
  filter(Age != "All") %>% 
  mutate(Age = as.integer(Age),
         Age_gr = case_when(Age <= 44 ~ "0",
                            Age >= 45 & Age <= 64 ~ "45",
                            Age >= 65 & Age <= 84 ~ "65",
                            Age >= 85 ~ "85")) %>% 
  group_by(Region, Date, Year, Week, t, Sex, Age_gr) %>% 
  summarise(Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  rename(Age = Age_gr) %>% 
  bind_rows(inters_pop %>% 
              filter(Age == "All"))

unique(pop3$Age)
unique(pop3$Region)
