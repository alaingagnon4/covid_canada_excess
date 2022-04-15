source("code/00_functions.R")

bsn <- 
  read_rds("data_inter/baseline_mortality_quebec.rds")

bsn10 <- 
  read_rds("data_inter/baseline_mortality_quebec_2010_2019.rds") %>% 
  select(age, date, bsn10 = bsn, lp10 = lp, up10 = up)


bsn2 <- 
  bsn %>% 
  select(age, date, exposure, dts, bsn, lp, up) %>% 
  left_join(bsn10)
  

exc15 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn,
         exc_lp = dts - up,
         exc_up = dts - lp) %>% 
  select(date, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess15")

exc10 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn10,
         exc_lp = dts - up10,
         exc_up = dts - lp10) %>% 
  select(date, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess10")

exc <- 
  bind_rows(exc15,
            exc10)

unique(exc$age)


# confirmed deaths =====
# ~~~~~~~~~~~~~~~~~~~~~~

qc <- 
  read_rds("data_inter/confirmed_deaths_quebec.rds") %>% 
  mutate(region = "Quebec") %>% 
  filter(sex == "t",
         format == "isq") %>% 
  select(-region, -sex, -dts, -format)

unique(qc$age)

conf_all_ages <- 
  # bind_rows(qc, ab, on) %>% 
  bind_rows(qc) %>% 
  group_by(date) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  mutate(age = "all")
  
conf <- 
  bind_rows(qc) %>% 
  mutate(age = age %>% as.character()) %>% 
  group_by(date, age) %>% 
  summarise(new = sum(new)) %>% 
  ungroup() %>% 
  bind_rows(conf_all_ages) %>% 
  mutate(source = "confirmed") 

all <- 
  exc %>% 
  rename(new = exc,
         new_lp = exc_lp,
         new_up = exc_up) %>% 
  bind_rows(conf)

unique(all$date)

all %>% 
  ggplot()+
  geom_ribbon(data = all %>% 
                filter(source %in% c("excess10", "excess15")),
              aes(date, ymin = new_lp, ymax = new_up, fill = source), 
              # col = "black",
              alpha = 0.1)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(~ age, scales = "free", ncol = 1)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  theme_bw()

ggsave("figures/comparison_confirmed_excess_quebec.pdf",
       w = 10,
       h = 15)

unique(qc$age)
unique(qc$sex)
unique(on$age)
unique(on$sex)
unique(ab$age)
unique(ab$sex)
