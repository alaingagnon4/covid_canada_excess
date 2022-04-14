source("code/00_functions.R")

bsn <- 
  read_rds("data_inter/baseline_mortality.rds")

unique(bsn$age)

exc <- 
  bsn %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn,
         exc_lp = dts - up,
         exc_up = dts - lp) %>% 
  select(date, region, sex, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess")

unique(exc$age)
unique(exc$region)

# confirmed deaths by province =====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

qc <- 
  read_rds("data_inter/confirmed_deaths_quebec.rds") %>% 
  mutate(region = "Quebec")
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
  mutate(age = "All")
  
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
  geom_ribbon(data = all %>% filter(sex == "t", source == "excess"),
              aes(date, ymin = new_lp, ymax = new_up), alpha = 0.3)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(age ~ region, scales = "free", ncol = 3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("red", "black"))+
  theme_bw()

ggsave("figures/comparison_confirmed_excess.pdf",
       w = 15,
       h = 10)


unique(qc$age)
unique(qc$sex)
unique(on$age)
unique(on$sex)
unique(ab$age)
unique(ab$sex)
