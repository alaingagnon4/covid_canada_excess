source("code/00_functions.R")

# loading population data from StatCan files
pop_prj <- read_csv("data_input/1710005701_databaseLoadingData.csv",
                    col_types = cols(.default = "c"))

pop_est <- read_csv("data_input/1710000501_databaseLoadingData.csv",
                    col_types = cols(.default = "c"))

# weeks and dates
# ~~~~~~~~~~~~~~~
weeks <- expand_grid(year = 2010:2023, week = 1:52) %>% 
  bind_rows(tibble(year = c(2014, 2020), week = 53)) %>% 
  arrange(year, week) %>% 
  mutate(year_week = paste0(year, "-", sprintf('%02d', week))) %>% 
  pull(year_week)

# population data
# ~~~~~~~~~~~~~~~~~~~~~~
unique(pop_est$`Age group`)
unique(pop_prj$`Age group`)

pop_est2 <- 
  pop_est %>% 
  rename(region = GEO,
         year = REF_DATE,
         age = 'Age group',
         sex = Sex,
         pop = VALUE) %>% 
  select(region, year, age, sex, pop) %>% 
  mutate(age = str_sub(age, 1, 3),
         age = case_when(age == "0 t" ~ "0", 
                         age == "5 t" ~ "5", 
                         age == "All" ~ "all", 
                         TRUE ~ age),
         sex = case_when(sex == "Both sexes" ~ "t",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         pop = as.integer(pop),
         year = as.integer(year),
         source = "estimates") 

pop_prj2 <- 
  pop_prj %>% 
  rename(region = GEO,
         year = REF_DATE,
         age = 'Age group',
         sex = Sex,
         pop = VALUE) %>% 
  select(region, year, age, sex, pop) %>% 
  # filter(age != "All ages") %>% 
  mutate(age = str_sub(age, 1, 3),
         age = case_when(age == "0 t" ~ "0", 
                         age == "5 t" ~ "5", 
                         age == "All" ~ "all", 
                         TRUE ~ age),
         sex = case_when(sex == "Both sexes" ~ "t",
                         sex == "Males" ~ "m",
                         sex == "Females" ~ "f"),
         pop = as.integer(pop) * 1000,
         year = as.integer(year),
         source = "projections")


# closing at age 90 for all years
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
popt <-
  pop_est2 %>%
  bind_rows(pop_prj2)
  
pop_all_age <- 
  popt %>% 
  filter(age == "all") 

pop <- 
  popt %>%
  filter(age != "all") %>% 
  mutate(age = age %>% as.integer(),
         age = ifelse(age < 90, age, 90)) %>% 
  group_by(region, sex, age, source, year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age = age %>% as.character()) %>% 
  bind_rows(pop_all_age)
  

# population growth in 2021-2023 in population projections
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_gth <- 
  pop %>% 
  filter(source == "projections") %>% 
  filter(year %in% 2021:2023) %>% 
  select(-source) %>%
  mutate(y = paste0("y", year)) %>% 
  select(-year) %>% 
  spread(y, pop) %>% 
  mutate(gth22 = y2022 / y2021,
         gth22 = ifelse(y2022 == y2021, 1, gth22),
         gth23 = y2023 / y2022,
         gth23 = ifelse(y2023 == y2022, 1, gth23)) %>% 
  select(-y2021, -y2022, -y2023) 

unique(pop_gth$region)
unique(pop_est2$age)

pop_22_23 <- 
  pop %>% 
  filter(source == "projections") %>% 
  select(-source) %>%
  filter(year == 2021) %>%
  left_join(pop_gth) %>% 
  mutate(
    p2022 = pop * gth22,
    p2023 = pop * gth22 * gth23
  ) %>% 
  select(region, age, sex, p2022, p2023) %>% 
  gather(p2022, p2023, key = y, value = pop)%>% 
  mutate(year = str_sub(y,2,5) %>% as.double()) %>% 
  select(-y)

pop2 <-
  pop %>% 
  filter(source == "estimates") %>% 
  select(-source) %>%
  filter(year %in% 2010:2021) %>%
  bind_rows(pop_22_23) %>% 
  arrange(region, sex, age, year) %>% 
  mutate(year_week = paste0(year, "-26"),
         age = str_trim(age)) %>%
  select(-year) %>%
  arrange(region, sex, age, year_week) %>% 
  complete(region, sex, age, year_week = weeks, fill = list(pop = NA)) %>%
  group_by(region, sex, age) %>%
  mutate(t = 1:n(),
         w = ifelse(is.na(pop), 0, 1),
         year = str_sub(year_week, 1, 4) %>% as.double(),
         week = str_sub(year_week, 6, 7) %>% as.double(),
         date = MMWRweek2Date(MMWRyear = year,
                              MMWRweek = week,
                              MMWRday = 7)) %>% 
  ungroup()


r <- "Quebec"
a <- "0"

pop2 %>% 
  filter(region == r,
         sex == "t",
         age == a) %>% 
  ggplot()+
  geom_point(aes(date, pop))+
  # facet_wrap(~region, scales = "free")+
  theme_bw()

interpop_fit_growth <- function(db)
{
  # including projections data
  xs1 <- db %>% drop_na() %>% pull(t)
  ys1 <- db %>% drop_na() %>% pull(pop)
  ts <- db %>% pull(t)
  pred_prj <- spline(xs1, ys1, xout = ts)$y
  
  # GAM interpolating 2010 - 2022
  gam_model <- 
    gam(pop ~ s(t, bs = "ps", m = c(2,2)),
        data = db,
        weights = w,
        family = quasipoisson(link = "log"))
  
  db %>% 
    mutate(pre_spl = pred_prj,
           pre_gam = predict(gam_model, 
                             type = "response", 
                             newdata = db))
}

# db <- 
#   pop2 %>% 
#   filter(age == "all", 
#          sex == "t") 

pop3 <- 
  pop2 %>% 
  group_by(region, age, sex) %>% 
  do(interpop_fit_growth(db = .data)) %>% 
  ungroup()

unique(pop3$year)


# comparison across estimates of population interpolation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a <- "0"
r <- "Quebec"
pop3 %>% 
  gather(starts_with("pre_"), key = type, value = pop_int) %>% 
  filter(sex == "t",
         region == r,
         age == a) %>% 
  ggplot()+
  geom_line(aes(date, pop_int, col = type))+
  geom_point(aes(date, pop))+
  # facet_wrap(~region, scales = "free")+
  theme_bw()


pop4 <- 
  pop3 %>%  
  mutate(exposure = pre_spl / 52) %>% 
  select(region, sex, age, date, exposure)

unique(pop4$age)

# population by age for Quebec data ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop5 <- 
  pop4 %>% 
  filter(age != "all") %>% 
  mutate(age = as.integer(age),
         age = case_when(age <= 44 ~ 0,
                         age %in% 45:64 ~ 45,
                         age %in% 65:84 ~ 65,
                         age %in% 85:110 ~ 85,
                         TRUE ~ NA_real_)) %>% 
  group_by(date, region, sex, age) %>% 
  summarise(exposure = sum(exposure)) %>% 
  ungroup() %>% 
  mutate(age = age %>% as.character()) %>% 
  bind_rows(pop4 %>% 
              filter(age == "all") %>% 
              select(date, region, sex, age, exposure))

unique(pop5$age)
unique(pop5$region)

write_rds(pop5, "data_inter/weekly_exposures_canada_age.rds")



# # previous approach: extra and intrapolating with splines ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pop1 <- 
#   pop %>% 
#   filter(!(source == "projections" & (year < 2023 | year > 2025))) %>%
#   mutate(year_week = paste0(year, "-26"),
#          age = str_trim(age)) %>%
#   select(-year) %>%
#   arrange(region, sex, age, year_week) %>% 
#   complete(region, age, sex, year_week = weeks, fill = list(pop = NA, source = "fill")) %>%
#   group_by(region, age, sex) %>%
#   mutate(t = 1:n(),
#          w = ifelse(is.na(pop), 0, 1),
#          year = str_sub(year_week, 1, 4),
#          week = str_sub(year_week, 6, 7),
#          isoweek = paste0(year, "-W", week, "-7"),
#          date = ISOweek2date(isoweek))
# 
# 
# # interpolating population over weeks ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # using cubic splines and GAM, with and without projections data 
# interpop_fits <- function(db)
# {
#   # including projections data
#   xs1 <- db %>% drop_na() %>% pull(t)
#   ys1 <- db %>% drop_na() %>% pull(pop)
#   ts <- db %>% pull(t)
#   pred_prj <- spline(xs1, ys1, xout = ts)$y
#   
#   # including only pop estimates until 2021
#   xs2 <- db %>% drop_na() %>% filter(year <= 2021) %>% pull(t)
#   ys2 <- db %>% drop_na() %>% filter(year <= 2021)  %>% pull(pop)
#   pred_est <- spline(xs2, ys2, xout = ts)$y
#   
#   # GAM including projections data
#   gam_model_prj <- 
#     gam(pop ~ s(t, bs = "ps", m = c(2,2)),
#         data = db,
#         weights = w,
#         family = quasipoisson(link = "log"))
#   
#   pred_gam_prj <- 
#     predict(gam_model_prj, 
#             type = "response", 
#             newdata = db)
#   
#   # GAM including only pop estimates until 2021
#   gam_model_est <- 
#     gam(pop ~ s(t, bs = "ps", m = c(2,2)),
#         data = db %>% 
#           mutate(w = ifelse(source == "projections", 0, w)),
#         weights = w,
#         family = quasipoisson(link = "log"))
#   
#   pred_gam_est <- 
#     predict(gam_model_est, 
#             type = "response", 
#             newdata = db)
#   
#   # "all together now!"
#   db %>% 
#     mutate(pre_prj = pred_prj,
#            pre_est = pred_est,
#            pre_gam_prj = pred_gam_prj,
#            pre_gam_est = pred_gam_est)
#   
# }
# # ====
# 
# pop2 <- 
#   pop1 %>% 
#   group_by(region, age, sex) %>% 
#   do(interpop_fits(db = .data)) %>% 
#   ungroup()
# 
# 
# # comparison across estimates of population interpolation
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# r <- c("Quebec", "Alberta", "Ontario")
# a <- "All"
# 
# pop2 %>% 
#   gather(starts_with("pre_"), key = type, value = pop_int) %>% 
#   filter(region %in% r,
#          sex == "t",
#          age == a) %>% 
#   ggplot()+
#   geom_line(aes(date, pop_int, col = type))+
#   geom_point(aes(date, pop, shape = source))+
#   facet_wrap(~region, scales = "free")+
#   theme_bw()
# 
# 
# # Most plausible option seems to be the GAM model ignoring the StatCan 
# # projection estimates
# 
# pop_test <-
#   pop_est2 %>%
#   bind_rows(pop_prj2) %>%
#   mutate(year_week = paste0(year, "-26"),
#          age = str_trim(age)) %>%
#   select(-year) %>%
#   arrange(region, sex, age, year_week) %>% 
#   complete(region, age, sex, year_week = weeks, fill = list(pop = NA, source = "fill")) %>%
#   group_by(region, age, sex) %>%
#   mutate(t = 1:n(),
#          w = ifelse(is.na(pop), 0, 1),
#          year = str_sub(year_week, 1, 4),
#          week = str_sub(year_week, 6, 7),
#          isoweek = paste0(year, "-W", week, "-7"),
#          date = ISOweek2date(isoweek))
# 
# 
# 
# pop_test %>%
#   filter(year < 2030) %>% 
#   gather(starts_with("pre_"), key = type, value = pop_int) %>% 
#   filter(region %in% r,
#          sex == "t",
#          age == a) %>% 
#   ggplot()+
#   # geom_line(aes(date, pop_int, col = type))+
#   geom_point(aes(date, pop, col = source))+
#   facet_wrap(~region, scales = "free")+
#   theme_bw()
# 
# 
# 
# pop3 <- 
#   pop2 %>%  
#   mutate(exposure = pre_gam_est / 52) %>% 
#   select(region, sex, age, isoweek, date, exposure)
# 
# 
# # population by age for Canada data ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pop4 <- 
#   pop3 %>% 
#   filter(age != "All") %>% 
#   mutate(age = as.integer(age),
#          age = case_when(age <= 44 ~ "0",
#                          age >= 45 & age <= 64 ~ "45",
#                          age >= 65 & age <= 84 ~ "65",
#                          age >= 85 ~ "85")) %>% 
#   group_by(region, isoweek, sex, age) %>% 
#   summarise(exposure = sum(exposure)) %>% 
#   ungroup() %>% 
#   bind_rows(pop3 %>% 
#               filter(age == "All") %>% 
#               select(region, isoweek, sex, age, exposure)) %>% 
#   mutate(exposure = round(exposure, 2))
# 
# unique(pop4$age)
# unique(pop4$region)
# 
# write_rds(pop4, "data_inter/weekly_exposures_province_age.rds")





