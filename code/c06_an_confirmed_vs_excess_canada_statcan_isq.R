source("code/00_functions.R")

bsn <- 
  read_rds("data_inter/baseline_mortality_canada_statcan_isq.rds")


bsn10 <- 
  read_rds("data_inter/baseline_mortality_canada_2010_2019_statcan_isq.rds") %>% 
  select(region, sex, age, date, bsn10 = bsn, lp10 = lp, up10 = up)

unique(bsn$region)
unique(bsn10$region)

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
  mutate(source = "excess_15_19")

exc10 <- 
  bsn2 %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(exc = dts - bsn10,
         exc_lp = dts - up10,
         exc_up = dts - lp10) %>% 
  select(date, region, sex, age, exc, exc_lp, exc_up) %>% 
  mutate(source = "excess_10_19")

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
  bind_rows(conf) %>% 
  filter(!(region == "Ontario" & date >= "2021-11-01"))

unique(all$date)

all %>% 
  filter(sex == "t",
         date <= "2021-12-31") %>% 
  ggplot()+
  geom_ribbon(data = all %>%
                filter(source %in% c("excess_10_19", "excess_15_19"),
                       sex == "t",
                       date <= "2021-12-31"),
              aes(date, ymin = new_lp, ymax = new_up, fill = source),
              alpha = 0.1)+
  geom_line(aes(date, new, col = source))+
  facet_wrap(age ~ region, scales = "free_y", ncol = 3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_fill_manual(values = c("blue", "black"), guide = "none")+
  theme_bw()

ggsave("figures/comparison_confirmed_excess_canada_statcan_isq.pdf",
       w = 15,
       h = 10)



unique(all$source)
all2 <- 
  all %>% 
  select(source, date, region, sex, age, new) %>% 
  spread(source, new) %>% 
  drop_na() %>% 
  mutate(ratio_log = ifelse(confirmed > 0 & excess_15_19 > 0, log(excess_15_19 / confirmed), log(1)),
         ratio = ifelse(confirmed > 0 & excess_15_19 > 0, excess_15_19 / confirmed, 1))

max(all2$ratio)
min(all2$ratio)



all2 %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio))+
  facet_wrap(region~.)+
  scale_fill_gradient2()+
  labs(title = "ratio excess / confirmed")+
  theme_bw()


all2 %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio_log))+
  facet_wrap(region~.)+
  scale_fill_gradient2()+
  labs(title = "ratio excess / confirmed")+
  theme_bw()



qt <- 6

col2 <- c(colorRampPalette(c("#03045e", "#caf0f8"), space = "Lab")(qt),
          "white",
          colorRampPalette(c("#fff0f3", "#c1121f"), space = "Lab")(qt))

# breaks_mc <- c(0, seq(0.5, 2, 0.1), 100)
breaks_mc <- c(0, seq(0.5, 0.9, 0.1), 
               0.98, 1.02,
               1/rev(seq(0.5, 0.9, 0.1)), 100)

all3 <- 
  all2 %>% 
  mutate(ratio_cut = cut(ratio, breaks = breaks_mc))

unique(all3$ratio_cut)

all3 %>% 
  filter(sex == "t") %>% 
  ggplot()+
  geom_tile(aes(date, age, fill = ratio_cut))+
  scale_fill_manual(values = col2)+
  facet_wrap(region~.)+
  # scale_fill_gradient2()+
  labs(title = "ratio excess / confirmed")+
  theme_bw()

ggsave("figures/heatmap_ratios.pdf",
       w = 10,
       h = 5)


# some examples
r <- c("Alberta", "Ontario", "Quebec")
a <- "85"
s <- "t"

bsn %>% 
  filter(region %in% r,
         sex == s) %>% 
  filter(!(region == "Ontario" & date >= "2021-11-01")) %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = lp, ymax = up), alpha = 0.2, fill = "#023e8a")+
  geom_point(aes(date, dts), size = 0.3)+
  geom_line(aes(date, dts), size = 0.4, alpha = 0.4)+
  geom_line(aes(date, bsn), size = 0.8, alpha = 0.7, col = "#023e8a")+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  theme_bw()+
  facet_wrap(age ~ region, scales = "free_y", ncol = 3)
ggsave("figures/baselines.pdf",
       w = 20,
       h = 10)

  
  # geom_ribbon(aes(date, ymin = incid_bsn_lp, ymax = incid_bsn_up), alpha = 0.2, fill = "#023e8a")+
  # geom_point(aes(date, incid, col = as.factor(w)), size = 1, alpha = 0.7)+
  # geom_line(aes(date, incid), size = 0.3, alpha = 0.5)+
  # geom_line(aes(date, incid_bsn), size = 0.5, alpha = 0.7, col = "#023e8a")+
  # geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  # scale_color_manual(values = c("red", "black"))+
  # facet_grid(agecat~sex, scales = "free_y")+
  # theme_bw()+
  # theme(legend.position = "none")



