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
  mutate(source = "excess_15_19")

exc10 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn10,
         exc_lp = dts - up10,
         exc_up = dts - lp10) %>% 
  select(date, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess_10_19")

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
                filter(source %in% c("excess_10_19", "excess_15_19")),
              aes(date, ymin = new_lp, ymax = new_up, fill = source), 
              # col = "black",
              alpha = 0.1)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(~ age, scales = "free", ncol = 1)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  theme_bw()

# ggsave("figures/comparison_confirmed_excess_quebec.pdf",
#        w = 10,
#        h = 15)

unique(qc$age)
unique(qc$sex)
unique(on$age)
unique(on$sex)
unique(ab$age)
unique(ab$sex)



# ratios age ====
# ~~~~~~~~~~~~~~~

pop <- 
  read_rds("weekly_deaths_exposures_quebec.rds") %>% 
  filter(date == "2021-01-30") %>% 
  select(age, exposure)

pop_wide <- 
  pop %>% 
  spread(age, exposure) %>% 
  mutate(mid_pop = `50` + `60`,
         old_pop = `80` + `90`) %>% 
  select(mid_pop, old_pop)
  
mid_pop <- pop_wide$mid_pop
old_pop <- pop_wide$old_pop

ratio_age_conf <- 
  all %>% 
  filter(source == "confirmed") %>% 
  mutate(new = new + 1) %>% 
  select(-new_lp, -new_up) %>% 
  spread(age, new) %>% 
  mutate(midage = `50` + `60`,
         old = `80` + `90`,
         old_pop = old_pop,
         mid_pop = mid_pop) %>% 
  mutate(ratio = (old/old_pop) / (midage/mid_pop))


ratio_age_conf %>% 
  ggplot(aes(date, ratio))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_y_log10()+
  scale_x_date(date_breaks = "3 month", date_labels = "%y%b")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  # 1
  geom_vline(xintercept = dmy("01-04-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-06-2020"), linetype = "dashed")+
  # 2
  geom_vline(xintercept = dmy("01-10-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-02-2021"), linetype = "dashed")+
  # 3
  geom_vline(xintercept = dmy("01-01-2022"), linetype = "dashed")+
  geom_vline(xintercept = dmy("01-04-2022"), linetype = "dashed")+
  theme_bw()+
  scale_color_manual(values = c("red", "black"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave("figures/ratios_age_conf_qc.pdf",
       w = 10,
       h = 5)

ggsave("figures/ratios_age_conf_qc.png",
       dpi = 600,
       w = 10,
       h = 5)

 
# with excess
ratio_age_exc <- 
  all %>% 
  filter(source == "excess_15_19") %>% 
  select(-new_lp, -new_up) %>%
  spread(age, new) %>% 
  mutate(midage = `50` + `60`,
         old = `80` + `90`,
         midage = ifelse(midage < 1, 1, midage),
         old = ifelse(old < 1, 1, old),
         old_pop = old_pop,
         mid_pop = mid_pop) %>% 
  mutate(ratio = (old/old_pop) / (midage/mid_pop))


ratio_age_exc %>% 
  ggplot(aes(date, ratio))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_y_log10()+
  scale_x_date(date_breaks = "3 month", date_labels = "%y%b")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  # 1
  geom_vline(xintercept = dmy("01-04-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-06-2020"), linetype = "dashed")+
  # 2
  geom_vline(xintercept = dmy("01-10-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-02-2021"), linetype = "dashed")+
  # 3
  geom_vline(xintercept = dmy("01-01-2022"), linetype = "dashed")+
  geom_vline(xintercept = dmy("01-04-2022"), linetype = "dashed")+
  theme_bw()+
  scale_color_manual(values = c("red", "black"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
ggsave("figures/ratios_age_excs_qc.pdf",
       w = 10,
       h = 5)

ggsave("figures/ratios_age_excs_qc.png",
       dpi = 600,
       w = 10,
       h = 5)

ratios <- 
  bind_rows(ratio_age_exc %>% select(date, ratio) %>% mutate(source = "excess"),
            ratio_age_conf %>% select(date, ratio) %>% mutate(source = "confirmed"))


ratios %>% 
  ggplot(aes(date, ratio, col = source))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_y_log10()+
  scale_x_date(date_breaks = "3 month", date_labels = "%y%b")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  # 1
  geom_vline(xintercept = dmy("01-04-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-06-2020"), linetype = "dashed")+
  # 2
  geom_vline(xintercept = dmy("01-10-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-02-2021"), linetype = "dashed")+
  # 3
  geom_vline(xintercept = dmy("01-01-2022"), linetype = "dashed")+
  geom_vline(xintercept = dmy("01-04-2022"), linetype = "dashed")+
  theme_bw()+
  scale_color_manual(values = c("red", "black"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


ggsave("figures/ratios_age_excs_conf_qc.pdf",
       w = 10,
       h = 5)

ggsave("figures/ratios_age_excs_conf_qc.png",
       dpi = 600,
       w = 10,
       h = 5)

