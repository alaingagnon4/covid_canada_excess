source("code/00_functions.R")

# loading population data from StatCan files
pop_prj <- read_xlsx("data_input/PopGrAS_Qc_majA2021.xlsx",
                     skip = 3)

pop_est <- read_xlsx("data_input/QC-age-sexe.xlsx",
                     skip = 3)

# weeks and dates
# ~~~~~~~~~~~~~~~
weeks <- expand_grid(year = seq(2010, 2025, 1), week = 1:52) %>% 
  bind_rows(tibble(year = c(2014, 2020), week = 53)) %>% 
  arrange(year, week) %>% 
  mutate(year_week = paste0(year, "-", sprintf('%02d', week))) %>% 
  pull(year_week)

# population data
# ~~~~~~~~~~~~~~~~~~~~~~

pop_est2 <- 
  pop_est %>% 
  rename(year = 1,
         sex = 2,
         all = 3) %>% 
  drop_na(sex) %>% 
  gather(-year, -sex, key = age, value = pop) %>% 
  separate(age, c("age", "tr")) %>% 
  filter(!age %in% c(""),
         year >= 2010) %>% 
  mutate(sex = recode(sex,
                      "1" = "m",
                      "2" = "f", 
                      "3" = "t"),
         source = "estimates",
         year = str_replace(year, "r|p", ""),
         year = year %>% as.double(),
         pop = pop %>% as.double()) %>% 
  select(-tr)

pop_prj2 <- 
  pop_prj %>% 
  select(year = 4,
         sex = 5,
         all = 6,
         13:33) %>% 
  filter(!is.na(sex)) %>% 
  gather(-year, -sex, key = age, value = pop) %>% 
  separate(age, c("age", "tr")) %>% 
  filter(!age %in% c(""),
         year >= 2022) %>% 
  mutate(sex = recode(sex,
                      "1" = "m",
                      "2" = "f", 
                      "3" = "t"),
         pop = pop %>% as.double(),
         source = "projections") %>% 
  select(-tr)

unique(pop_prj2$age)
  
pop <-
  pop_est2 %>%
  bind_rows(pop_prj2) %>%
  filter(!(source == "projections" & (year < 2023 | year > 2025))) %>%
  mutate(year_week = paste0(year, "-26"),
         age = str_trim(age)) %>%
  select(-year) %>%
  arrange(sex, age, year_week) %>% 
  complete(age, sex, year_week = weeks, fill = list(pop = NA, source = "fill")) %>%
  group_by(age, sex) %>%
  mutate(t = 1:n(),
         w = ifelse(is.na(pop), 0, 1),
         year = str_sub(year_week, 1, 4),
         week = str_sub(year_week, 6, 7),
         isoweek = paste0(year, "-W", week, "-7"),
         date = ISOweek2date(isoweek))


# interpolating population over weeks ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using cubic splines and GAM, with and without projections data 
interpop <- function(db)
{
  # including projections data
  xs1 <- db %>% drop_na() %>% pull(t)
  ys1 <- db %>% drop_na() %>% pull(pop)
  ts <- db %>% pull(t)
  pred_prj <- spline(xs1, ys1, xout = ts)$y
  
  # including only pop estimates until 2021
  xs2 <- db %>% drop_na() %>% filter(year <= 2021) %>% pull(t)
  ys2 <- db %>% drop_na() %>% filter(year <= 2021)  %>% pull(pop)
  pred_est <- spline(xs2, ys2, xout = ts)$y
  
  # GAM including projections data
  gam_model_prj <- 
    gam(pop ~ s(t, bs = "ps", m = c(2,2)),
        data = db,
        weights = w,
        family = quasipoisson(link = "log"))
  
  pred_gam_prj <- 
    predict(gam_model_prj, 
            type = "response", 
            newdata = db)
  
  # GAM including only pop estimates until 2021
  gam_model_est <- 
    gam(pop ~ s(t, bs = "ps", m = c(2,2)),
        data = db %>% 
          mutate(w = ifelse(source == "projections", 0, w)),
        weights = w,
        family = quasipoisson(link = "log"))
  
  pred_gam_est <- 
    predict(gam_model_est, 
            type = "response", 
            newdata = db)
  
  # "all together now!"
  db %>% 
    mutate(pre_prj = pred_prj,
           pre_est = pred_est,
           pre_gam_prj = pred_gam_prj,
           pre_gam_est = pred_gam_est)
  
}
# ====

pop2 <- 
  pop %>% 
  group_by(age, sex) %>% 
  do(interpop(db = .data)) %>% 
  ungroup() 


# comparison across estimates of population interpolation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# r <- c("Quebec", "Alberta", "Ontario")
a <- "0"
a <- c("0", "50", "70", "90")
pop2 %>% 
  gather(starts_with("pre_"), key = type, value = pop_int) %>% 
  filter(sex == "t",
         age %in% a) %>% 
  ggplot()+
  geom_line(aes(date, pop_int, col = type))+
  geom_point(aes(date, pop, shape = source))+
  facet_wrap(~age, scales = "free")+
  theme_bw()

ggsave("figures/population_projections_qc.pdf",
       w = 12,
       h = 4)
# Most plausible option seems to be the GAM model ignoring the StatCan 
# projection estimates

pop3 <- 
  pop2 %>%  
  mutate(exposure = pre_gam_est / 52) %>% 
  select(sex, age, isoweek, date, exposure)

unique(pop3$age)
# population by age for Quebec data ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop4 <- 
  pop3 %>% 
  filter(age != "all") %>% 
  mutate(age = as.integer(age),
         age = case_when(age <= 49 ~ "0",
                         age %in% 50:59 ~ "50",
                         age %in% 60:69 ~ "60",
                         age %in% 70:79 ~ "70",
                         age %in% 80:89 ~ "80",
                         age >= 90 ~ "90")) %>% 
  group_by(isoweek, sex, age) %>% 
  summarise(exposure = sum(exposure)) %>% 
  ungroup() %>% 
  bind_rows(pop3 %>% 
              filter(age == "all") %>% 
              select(isoweek, sex, age, exposure)) %>% 
  mutate(exposure = round(exposure, 2),
         year = year %>% as.double())

unique(pop4$age)
write_rds(pop4, "data_inter/weekly_exposures_quebec_age.rds")
