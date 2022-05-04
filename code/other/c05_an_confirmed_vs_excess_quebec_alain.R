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
  mutate(
    age=case_when(age=="0" ~ "0-49",
                  age=="50" ~ "50-59",
                  age=="60" ~ "60-69",
                  age=="70" ~ "70-79",
                  age=="80" ~ "80-89",
                  age=="90" ~ "90+",
                  age=="all" ~ "all")) %>% 
  mutate(source = "confirmed") 

all <- 
  exc %>% 
  rename(new = exc,
         new_lp = exc_lp,
         new_up = exc_up) %>% 
  filter(source=="excess_15_19") %>%
  mutate(
    age=case_when(age=="0" ~ "0-49",
                  age=="50" ~ "50-59",
                  age=="60" ~ "60-69",
                  age=="70" ~ "70-79",
                  age=="80" ~ "80-89",
                  age=="90" ~ "90+",
                  age=="all" ~ "all")) %>% 
  bind_rows(conf)

unique(all$date)

all %>% 
  ggplot()+
  geom_ribbon(data = all %>% 
                filter(source %in% c("excess10", "excess_15_19")),
              aes(date, ymin = new_lp, ymax = new_up, fill = source), 
              # col = "black",
              alpha = 0.1)+
  scale_fill_discrete(name = "Source", labels = c("Décès confirmés", "Surmortalité"))+  geom_line(aes(date, new, col = source))+
  facet_wrap(~ age, scales = "free", ncol = 2)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_date(date_breaks = "3 month")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  
  theme_bw()

ggsave("figures/comparison_confirmed_excess_quebec.png",dpi=600,
       w = 15,
       h = 10)

unique(qc$age)
unique(qc$sex)
unique(on$age)
unique(on$sex)
unique(ab$age)
unique(ab$sex)



#####################Heat map#################

# unique(all$source)
# all2 <- 
#   all %>% 
#   select(source, date, age, new) %>% 
#   spread(source, new) %>% 
#   drop_na() %>% 
#   mutate(ratio_log = ifelse(confirmed > 0 & excess_15_19 > 0, log(excess_15_19 / confirmed), log(0.000001)),
#          ratio = ifelse(confirmed > 0 & excess_15_19 > 0, excess_15_19 / confirmed, 0.000001))
# 
# max(all2$ratio)
# min(all2$ratio)

unique(all$source)
all2 <- 
  all %>% 
  select(source, date, age, new) %>% 
  spread(source, new) %>% 
  drop_na() %>% 
  mutate(ratio_log = log(excess_15_19 / confirmed),
         ratio = ifelse(confirmed > 0 & excess_15_19 > 0, excess_15_19 / confirmed, NA))

max(all2$ratio)
min(all2$ratio)


all2 %>% 
#  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio))+
  #facet_wrap(region~.)+
  scale_fill_gradient2()+
  labs(title = "Ratio surmortalité / décès COVID")+
  theme_bw()


all2 %>% 
#  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio_log))+
#  facet_wrap(region~.)+
  scale_fill_gradient2()+
  labs(title = "Ratio surmortalité / décès COVID")+
  theme_bw()



qt <- 6

# col2 <- c(colorRampPalette(c("#03045e", "#caf0f8"), space = "Lab")(qt),
#           "white",
#           colorRampPalette(c("#fff0f3", "#c1121f"), space = "Lab")(qt))


col2 <- c(colorRampPalette(c("#c1121f", "#fff0f3"), space = "Lab")(qt),
          "white",
          colorRampPalette(c("#caf0f8", "#03045e"), space = "Lab")(qt))

# breaks_mc <- c(0, seq(0.5, 2, 0.1), 100)
breaks_mc <- c(0, seq(0.5, 0.9, 0.1), 
               0.91, 1.1,
               1/rev(seq(0.5, 0.9, 0.1)), 100)

all3 <- 
  all2 %>% 
  mutate(ratio_cut = cut(ratio, breaks = breaks_mc))

unique(all3$ratio_cut)

all3 %>% 
#  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio_cut))+
  scale_fill_manual(values = col2, na.value = 'grey90')+
#  facet_wrap(region~.)+
  # scale_fill_gradient2()+
  labs(title = "Ratio surmortalité / décès COVID")+
  labs(fill="Ratio")+
  scale_x_date(date_breaks = "6 month")+
  theme_bw()
  

ggsave("figures/heatmap_ratios_quebec.png", dpi=600,
       w = 5,
       h = 5)

###Ratios âges vieux/moins vieux
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


ratio_age_exc <- 
  all %>% 
  filter(source == "excess15") %>% 
  select(-new_lp, -new_up) %>%
  spread(age, new)

ratio_age_conf <- 
  all %>% 
  filter(source == "confirmed") %>% 
  mutate(new = new + 1) %>% 
  select(-new_lp, -new_up) %>% 
  spread(age, new) %>% 
  mutate(midage = `50` + `60`,
         old = `80` + `90`) %>% 
  left_join(pop_wide) %>% 
  mutate(ratio = (old/old_pop) / (midage/mid_pop))


ratio_age_conf %>% 
  ggplot(aes(date, ratio))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_y_log10()+
  scale_x_date(date_breaks = "3 month")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  # 1
  geom_vline(xintercept = dmy("01-04-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-06-2020"), linetype = "dashed")+
  # 2
  geom_vline(xintercept = dmy("01-10-2020"), linetype = "dashed")+
  geom_vline(xintercept = dmy("15-03-2021"), linetype = "dashed")+
  # 3
  geom_vline(xintercept = dmy("01-01-2022"), linetype = "dashed")+
  geom_vline(xintercept = dmy("01-04-2022"), linetype = "dashed")+
  theme_bw()





##########################

ratio_age_exc <- 
all %>% 
filter=(source=="excess_15_19") %>% 
  select(-new_lp, -new_up) %>%
  spread (age,new)

ratio_age_conf <- 
  all %>% 
  filter(source == "confirmed") %>% 
  select(-new_lp, -new_up) %>% 
  spread(age, new) %>% 
  mutate(midage = `50` + `60` + `70`,
         old= `90` + `80`,
         ratio = old / midage)



ggplot()+
  geom_ribbon(data = all %>% 
             ,
              aes(date, ymin = new_lp, ymax = new_up, fill = source), 
              # col = "black",
              alpha = 0.1)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(~ age, scales = "free", ncol = 1)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_date(date_breaks = "3 month")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  theme_bw()

ggsave("figures/comparison_confirmed_excess_quebec.png",dpi=600,
       w = 10,
       h = 15)

