source("code/00_functions.R")

bsn <- 
  read_rds("data_inter/baseline_mortality_canada.rds")

bsn10 <- 
  read_rds("data_inter/baseline_mortality_canada_2010_2019.rds") %>% 
  select(region, sex, age, date, bsn10 = bsn, lp10 = lp, up10 = up)


bsn2 <- 
  bsn %>% 
  select(region, sex, age, date, exposure, dts, bsn, lp, up) %>% 
  left_join(bsn10)


exc15 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn,
         exc_lp = dts - up,
         exc_up = dts - lp) %>% 
  select(date, region, sex, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess15")

exc10 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn10,
         exc_lp = dts - up10,
         exc_up = dts - lp10) %>% 
  select(date, region, sex, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess10")

exc <- 
  bind_rows(exc15,
            exc10)


# confirmed deaths by province =====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

qc <- 
  read_rds("data_inter/confirmed_deaths_quebec.rds") %>% 
  mutate(region = "Quebec",
         age = age %>% as.character()) %>% 
  filter(format == "statcan") %>% 
  select(-format) 

ab <- 
  read_rds("data_inter/confirmed_deaths_alberta.rds") %>% 
  mutate(region = "Alberta")

on <- 
  read_rds("data_inter/confirmed_deaths_ontario.rds") %>% 
  mutate(region = "Ontario")


conf_all_ages <- 
  bind_rows(qc, ab, on) %>% 
  group_by(date, region, sex) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(age = "all")
  
conf <- 
  bind_rows(qc, ab, on) %>% 
  mutate(age = age %>% as.character(),
         age = ifelse(age == "75", "65", age)) %>% 
  group_by(date, region, sex, age) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  bind_rows(conf_all_ages) %>% 
  mutate(source = "confirmed") 

rgs <- c("Alberta", "Ontario", "Quebec")

all <- 
  exc %>% 
  rename(new = exc,
         new_lp = exc_lp,
         new_up = exc_up) %>% 
  filter(region %in% rgs) %>% 
  bind_rows(conf)

unique(all$date)

all %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_ribbon(data = all %>%
                filter(source %in% c("excess10", "excess15"),
                       sex == "t"),
              aes(date, ymin = new_lp, ymax = new_up, fill = source),
              alpha = 0.1)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(age ~ region, scales = "free", ncol = 3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  theme_bw()

ggsave("figures/comparison_confirmed_excess_canada.pdf",
       w = 15,
       h = 10)


