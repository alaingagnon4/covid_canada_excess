source("code/00_functions.R")

prv_keep <- 
  c("Canada", "Quebec", "Ontario", 
    "Manitoba", "Saskatchewan", "Alberta",
    "British Columbia")

dts <- 
  read_rds("weekly_deaths_exposures_canada_statcan_isq.rds") %>% 
  # mutate(week = str_sub(isoweek, 7,8),
  #        week = week %>% as.double(),
  #        year = epiyear(date)) %>% 
  filter(region %in% prv_keep) %>% 
  mutate(week = epiweek(date))

unique(dts$region)


# Specifications for fitting the mortality baseline
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # period (initial and last date) of fitting
  id_f = "2015-01-01"
  ld_f = "2020-03-01"
  # weeks to be excluded before and after the seasonal peak
  exc_wks_ba_peak = 2
  # specific weeks to be systematically excluded in all years,
  # if only weeks detected as outliers or appertaining to the seasonal peak
  # want to be excluded, then this parameter should be "c()"
  exc_wks = c(1)
  # period (initial and last date) of prediction
  id_p = "2015-01-01"
  ld_p = "2022-06-30"
  # whether automatically excluding or not outliers
  # if T (i.e., True), then it makes two-step process, if F (i.e., False), 
  # then it doesn't evaluate nor exclude outliers and makes one-step fitting
  exc_outlrs = T
  # threshold to exclude outliers, if exc_outlrs = F, then it is not accounted
  outlrs_thld = 0.9
  # type of secular trend component: 
  # if linear, it is possible to select a polynomial function of order 
  # between 1 and 5, as : "linear_1" ... "linear_5" 
  # if nonlinear, it fits a cubic p-spline as: "pspline"
  sec_trend_comp = "linear_1"
  # type of seasonal component: 
  # if linear, it is possible to select sinusoidal terms 
  # between 1 and 4 components, as : "sincos_1" ... "sincos_4" 
  # if nonlinear, it fits a cyclic p-spline as: "cyc_pspline"
  seasonal_comp = "cyc_pspline"
  # uncertainty level of the prediction intervals
  pred_invals = 0.95
}

# Estimation of baseline mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bsn <- 
  dts %>% 
  mutate(year = year(date)) %>% 
  group_by(region, sex, age) %>% 
  do(estimate_baseline(db_in = .data,
                       id_f,
                       ld_f,
                       exc_wks_ba_peak,
                       exc_wks,
                       id_p,
                       ld_p,
                       exc_outlrs,
                       outlrs_thld,
                       sec_trend_comp,
                       seasonal_comp,
                       pred_invals)) %>% 
  ungroup()

write_rds(bsn, "data_inter/baseline_mortality_canada_statcan_isq.rds")

# test using 2010-2019 data
id_f <- "2010-01-01"
ld_f <- "2020-03-01"
id_p <- "2010-01-01"

bsn <- 
  dts %>% 
  mutate(year = year(date)) %>% 
  # group_by(region, sex, age) %>% 
  group_by(region, sex, age) %>% 
  do(estimate_baseline(db_in = .data,
                       id_f,
                       ld_f,
                       exc_wks_ba_peak,
                       exc_wks,
                       id_p,
                       ld_p,
                       exc_outlrs,
                       outlrs_thld,
                       sec_trend_comp,
                       seasonal_comp,
                       pred_invals)) %>% 
  ungroup()

write_rds(bsn, "data_inter/baseline_mortality_canada_2010_2019_statcan_isq.rds")

# # test run using loops to identify problematic cases ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rgs <- unique(dts$region)
# sxs <- unique(dts$sex)
# ags <- unique(dts$age)
# 
# bsn_test <- tibble()
# 
# for(r in rgs){
#   for(s in sxs){
#     for(a in ags){
#       
#       chunk <-
#         dts %>%
#         filter(region == r,
#                sex == s,
#                age == a)
#       
#       temp <-
#         estimate_baseline(
#           db_in = chunk,
#           id_f,
#           ld_f,
#           exc_wks_ba_peak,
#           exc_wks,
#           id_p,
#           ld_p,
#           exc_outlrs,
#           outlrs_thld,
#           sec_trend_comp,
#           seasonal_comp,
#           pred_invals
#         )
#       
#       bsn_test <-
#         temp %>% 
#         mutate(region = r,
#                sex = s,
#                age = a) %>% 
#         select(region, sex, age, everything()) %>% 
#         bind_rows(bsn_test)
#     }
#   }
# }
# 
# write_rds(bsn_test, "data_inter/baseline_mortality.rds")

# # test run of function for one combination province/sex/age ====
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# a <- "85"
# s <- "m"
# r <- "Canada"
# 
# chunk <-
#   dts %>%
#   filter(age == a,
#          sex == s,
#          region == r)
# 
# baselines <-
#   estimate_baseline(
#     chunk,
#     id_f,
#     ld_f,
#     exc_wks_ba_peak,
#     exc_wks,
#     id_p,
#     ld_p,
#     exc_outlrs,
#     outlrs_thld,
#     sec_trend_comp,
#     seasonal_comp,
#     pred_invals)
# 
# 
# baselines %>%
#   ggplot()+
#   geom_ribbon(aes(date, ymin = lp, ymax = up), alpha = 0.3)+
#   geom_line(aes(date, dts))+
#   geom_line(aes(date, bsn))+
#   theme_bw()+
#   labs(title = paste0(r, "_sex_", s, "_age_", a))
# 
# # =====
