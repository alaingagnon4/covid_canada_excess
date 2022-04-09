# functions script
# project: Covid Canada excess analysis
# =====================================
# =====================================

library(tidyverse)
library(readxl)
library(lubridate)
library(ISOweek)
library(mgcv)
library(ungroup)

options(scipen=999)

# loading list with CDC weeks by year
weeks_year <- 
  read_csv("data_input/cdc_weeks_per_year_1974_2040.csv")

weeks_dates <- 
  tibble(date = seq(ymd('2010-01-09'),ymd('2022-12-31'), by = '1 week'),
         year = epiyear(date),
         week = isoweek(date))

# age harmonization in the same age groups as StatCan ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
harmonize_age_statcan <- function(db, lambda = 100){
  
  age <- db$age %>% as.integer()
  dts <- db$dts
  nlast <- 101 - max(age)
  
  # without offset
  V1 <- pclm(x = age, 
             y = dts, 
             # offset = pops,
             nlast = nlast)$fitted
  
  out <- 
    tibble(age = seq(0, 100, 1), dts = V1) %>% 
    mutate(age = case_when(age <= 44 ~ 0,
                           age %in% 45:64 ~ 45,
                           age %in% 65:84 ~ 65,
                           age %in% 85:110 ~ 85,
                           TRUE ~ NA_real_)) %>% 
    group_by(age) %>% 
    summarise(dts = sum(dts))
  
  return(out)
}

# db_d <- temp1
# d_ini = "2015-01-01"
# d_end = "2020-03-31"

# Identify seasonal peaks by age to calibrate classic Serfling ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indetify_week_peaks <- 
  function(db_d = temp1, 
           d_ini = "2015-01-01",
           d_end = "2022-12-31"){
    
    sea_spl <- 
      db_d %>% 
      arrange(date) %>% 
      filter(date >= d_ini & date <= d_end) %>% 
      mutate(t = 1:n())
      
    gam_model <- 
      gam(dts ~ s(t, bs = "ps", m = c(2,2)) + 
            s(week, bs = 'cp'),
            offset(log(exposure)),
          data = sea_spl, 
          family = quasipoisson(link = "log"))
    
    sea_spl_out <- 
      sea_spl %>% 
      mutate(dts_sm = predict(gam_model, 
                              type = "response", 
                              newdata = sea_spl))
    
    
    peak_weeks <- 
      sea_spl_out %>%
      mutate(ep_year = ifelse(week > 26, year(date), year(date)-1)) %>% 
      group_by(ep_year) %>% 
      filter(dts_sm == max(dts_sm)) %>% 
      ungroup() %>% 
      group_by(week) %>% 
      summarise(n = n(), .groups = 'drop') %>% 
      ungroup()
    
    peak_week_age <- 
      peak_weeks %>% 
      filter(n == max(n)) %>% 
      # guarantying there is only one peak by age
      summarise(week = mean(week),
                n = mean(n), 
                .groups = 'drop') %>% 
      select(-n, peak_week = week)
    
    return(peak_week_age)
  }
# ====


# function for bootstrapping 
# ==========================

# bootstrapping process, adapted from Jonas schoeley's method 
# https://github.com/jschoeley/rbx2020
simul_intvals <- 
  function(
    # fitted model 
    model, 
    # either GLM or GAM (needed for model matrix extraction step)
    model_type, 
    # prediction data
    db, 
    # number of iterations
    nsim, 
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # offsets extracted directly from the prediction data
    offset_prd <- matrix(log(db$exposure))
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim, 
                              coef(model), 
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b + offset_prd))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu) 
      mu_ <- mu[!idx_na] 
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <- 
      db %>% 
      select(date)
    
    colnames_y_sim <- paste0('dts_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('dts_sim'),
                   names_to = 'sim_id', values_to = 'dts_sim') %>%
      group_by(date) %>%
      summarise(
        lp = quantile(dts_sim, lp, na.rm = TRUE),
        up = quantile(dts_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }



# db_in <-
#   dts %>%
#   filter(age == "85",
#          region == "Canada",
#          sex == "m")


# Generalized function for baseline fitting and prediction
# ========================================================
estimate_baseline <- 
  function(
    db_in = db_in,
    id_f = "1976-01-01",
    ld_f = "2020-03-01",
    exc_wks_ba_peak = 6,
    exc_wks = c(),
    id_p = "1976-01-01",
    ld_p = "2020-12-31",
    exc_outlrs = T,
    outlrs_thld = 0.8,
    sec_trend_comp = "pspline",
    seasonal_comp = "sincos_1",
    pred_invals = 0.95,
    save_plot = T,
    save_csv = T
  ){
    
    # define periods to fit and predict (train and prediction data)
    fit_per <- 
      seq(ymd(id_f), ymd(ld_f), 
          by = "day")
    
    pred_per <- 
      seq(ymd(id_p), ymd(ld_p), 
          by = "day")
    
    
    # Preparing data for fitting ====
    # ===============================
    
    # computing exposures in weekly person-years
    # adding the time variable 't'
    temp1 <- 
      db_in %>% 
      # creating a time variable (along the weeks) 
      arrange(date) %>% 
      mutate(t = 1:n()) # definition of variable t for the secular trend

    # Identifying peak weeks for adjust sinusoidal of classic Serfling.
    # The amount of knots defines level of flexibility here, and the seasonal 
    # peak is sensitive to this parameter. If knots are not defined (i.e., NA 
    # in this parameter), the amount of knots are automatically defined by the 
    # p-spline mcgv algorithm.
    # If 3 knots are selected, the seasonal pattern is very similar to a 
    # parametric linear term with one sinusoidal curve (i.e., sn2 + cs2) 
    peak_week_age <- 
      indetify_week_peaks(temp1, id_f, ld_f) %>% pull(peak_week) %>% round()
    
    # Periods that should be excluded in each age, according to the option 
    # selected in the 'exc_wks_ba_peak' function parameter
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # identifying periods t where peak weeks are located in age a
    peak_t_sex_age <- 
      temp1 %>% 
      select(t, week) %>% 
      mutate(peak_week = peak_week_age) %>% 
      filter(week == peak_week) %>% 
      select(t)
    
    # setting an empty tibble
    exc_wks_ba_peak_age <- tibble()
    
    peak_t <- 
          peak_t_sex_age  %>% 
          pull(t)
    
    ts <- NULL
    for(i in 1:length(peak_t)){
      t_temp <- c((peak_t[i] - exc_wks_ba_peak):(peak_t[i] + exc_wks_ba_peak))
      ts <- c(ts, t_temp)
    }
    
    exc_wks_ba_peak_age <- 
      temp1 %>% 
      select(t) %>% 
      mutate(seas_peak = ifelse(t %in% ts, 1, 0)) %>% 
      bind_rows(exc_wks_ba_peak_age)
    
    # data for fitting the baseline
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    db_d2 <- 
      temp1 %>% 
      # Including information of the amount of weeks per year to interpolate the
      # sinusoidal curve within each year, accounting for leap years
      left_join(weeks_year, by = "year") %>%
      # adding the indication of peak periods
      left_join(exc_wks_ba_peak_age, by = c("t")) %>% 
      # Adjusting the weeks to calibrate the peak of sinusoidal curves by age
      # as sin/cos terms alone define the seaonal peak in weeks 6-7 by default, 
      # which is not always the case 
      mutate(
        # Including the information of seasonal peak to calibrate the sinusoidal 
        # components in each age
        peak_week = peak_week_age,
        week_serf = week - peak_week + 6,
             # Adding quadratic and cubic terms for a polynomial secular trend
             # of degrees 2 and 3
             t2 = t^2,
             t3 = t^3,
             t4 = t^4,
             t5 = t^5,
             # Creating the sinusoidal linear base for a parametric seasonal
             # component
             # The seasonal terms, are based on the calibrated peak week (week_serf)
             sn2 = sin((2*pi*week_serf)/weeks_year),
             cs2 = cos((2*pi*week_serf)/weeks_year),
             sn4 = sin((4*pi*week_serf)/weeks_year),
             cs4 = cos((4*pi*week_serf)/weeks_year),
             sn8 = sin((8*pi*week_serf)/weeks_year),
             cs8 = cos((8*pi*week_serf)/weeks_year),
             sn10 = sin((10*pi*week_serf)/weeks_year),
             cs10 = cos((10*pi*week_serf)/weeks_year),
             # defining weights to define which observations are included in 
             # the fitting (train data)
             ws = ifelse(
               # time frame to fit
               date %in% fit_per & 
                 # excluding weeks defined by user 
                 !week %in% exc_wks & 
                 # excluding seasonal peaks according to the user specification 
                 seas_peak == 0 & 
                 # excluding those periods in which the either deaths 
                 # or exposures are missing 
                 !is.na(dts) & 
                 !is.na(exposure), 
               1, 0),
             # defining time frame to include in the prediction (prediction data)
             to_pred = ifelse(date %in% pred_per, 1, 0),
        log_exposure = log(exposure)) %>% 
      select(sex, age, t, date, week, dts, exposure, log_exposure, 
             peak_week, week_serf, seas_peak, ws, to_pred,
             t2, t3, t4, t5, 
             sn2, cs2, sn4, cs4, sn8, cs8, sn10, cs10)
    
    # model specification ====
    # ===================
    # according to function parameters defined by user
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Defining between GLM and GAM for the type of model to use
    if(str_detect(sec_trend_comp, "linear") == TRUE &
       str_detect(seasonal_comp, "sincos") == TRUE){
      # If both secular trend and seasonal components are linear, then use GLM,
      model_type <- "glm"
    }else{
      # if any of both is semiparametric (p-splines, in this context), use GAM
      model_type <- "gam"
    }
    
    # defining secular trend component
    sec_t <- 
      case_when(sec_trend_comp == "linear_1" ~ "t",
                sec_trend_comp == "linear_2" ~ "t + t2",
                sec_trend_comp == "linear_3" ~ "t + t2 + t3",
                sec_trend_comp == "linear_4" ~ "t + t2 + t3 + t4",
                sec_trend_comp == "linear_5" ~ "t + t2 + t3 + t4 + t5",
                sec_trend_comp == "pspline" ~ "s(t, bs = 'ps', m = c(2,2))")
    
    # defining seasonal component
    sea_t <- 
      case_when(seasonal_comp == "sincos_1" ~ "sn2 + cs2",
                seasonal_comp == "sincos_2" ~ "sn2 + cs2 + sn4 + cs4",
                seasonal_comp == "sincos_3" ~ "sn2 + cs2 + sn4 + cs4 + sn8 + cs8",
                seasonal_comp == "sincos_4" ~ "sn2 + cs2 + sn4 + cs4 + sn8 + cs8 + sn10 + cs10",
                seasonal_comp == "cyc_pspline" ~ "s(week, bs = 'cp')")
    
    # Defining model formula
    f <- paste0("dts ~ ", sec_t, " + ", sea_t)
    
    # fitting and prediction!!!!! ====
    # ================================
    
    # defining predicting data
    # ~~~~~~~~~~~~~~~~~~~~~~~~
    db_to_pred <- 
      db_d2 %>% 
      filter(to_pred == 1) 
    
    # defining train data
    # ~~~~~~~~~~~~~~~~~~~
    # when one-step fitting process, without identifying outliers
    if(exc_outlrs == F){ 
      
      db_to_fit <- 
        db_d2 
      
      # define all observations as no-outliers
      outlrs <- 
        db_to_fit %>% 
        mutate(outlier = 0) %>% 
        select(date, outlier)
      
          
    }else{ # when identifying outliers in a two-step fitting process
      
      # defining data to include in the fitting
      db_to_fit1 <- 
        db_d2 %>% 
        filter(date >= id_f & date <= ld_f)
      
      # first-step fitting for identifying outliers
      model1 <- 
        do.call(model_type, 
                list(as.formula(f), 
                     data = as.name("db_to_fit1"), 
                     offset = as.name("log_exposure"),
                     weights = as.name("ws"),
                     family = quasipoisson(link = "log"),
                     na.action = "na.omit"))
      
      # model1 <- 
      #   do.call(model_type, 
      #           list(paste0(as.formula(f), ", offset(log(exposure))"), 
      #                data = as.name("db_to_fit1"), 
      #                # offset = log(as.double(as.name("exposure"))),
      #                weights = as.name("ws"),
      #                family = quasipoisson(link = "log"),
      #                na.action = "na.omit"))
      
      # first-step model prediction
      pred1 <- predict(model1, type = "response", newdata = db_to_pred)
      
      # identifying outliers
      outlrs <- 
        db_to_pred %>% 
        mutate(bsn = pred1 * exposure) %>% 
        # bootstrapping prediction intervals at specified threshold, 
        # with 2000 iterations
        left_join(simul_intvals(model1, 
                                model_type, 
                                db_to_pred, 
                                500, 
                                outlrs_thld),
                  by = "date") %>% 
        mutate(outlier = ifelse(dts > up & !is.na(dts), 1, 0)) %>% 
        select(date, outlier)
      
      # defining the train data, excluding outliers identified in the first-step 
      db_to_fit <- 
        db_d2 %>% 
        left_join(outlrs,
                  by = "date") %>% 
        mutate(ws = ifelse(ws == 1 & outlier == 0, 1, 0))
      
    }
    
    
    # fitting the model
    # ~~~~~~~~~~~~~~~~~
    model <- 
      do.call(model_type, 
              list(as.formula(f), 
                   data = as.name("db_to_fit"), 
                   offset = as.name("log_exposure"), 
                   weights = as.name("ws"),
                   family = quasipoisson(link = "log"),
                   na.action = "na.omit"))
    
    # predicting mortality baseline estimates
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pred <- predict(model, type = "response", newdata = db_to_pred)
    
    db_bsln_a <- 
      db_to_pred %>% 
      left_join(outlrs,
                by = "date") %>% 
      mutate(ws = ifelse(ws == 1 & outlier == 0, 1, 0),
             bsn = pred * exposure) %>% 
      left_join(simul_intvals(model, 
                              model_type, 
                              db_to_pred, 
                              500, 
                              pred_invals),
                by = "date")
    
    # appending to other ages estimates
    db_bslns <- 
      db_bsln_a 
    
    
    
    # output_data tibble with estimates
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    db_out <- 
      db_bslns %>% 
      select(t, date, week, exposure, dts, ws, to_pred,
             seas_peak, outlier, bsn, lp, up) %>% 
      mutate(model_spec = paste0(sec_trend_comp, "_", seasonal_comp),
             exc_wks_ba_peak = exc_wks_ba_peak,
             exc_outlrs = paste0(exc_outlrs, "_at_", outlrs_thld),
             pred_invals = pred_invals,
             ini_date_f = id_f,
             lst_date_f = ld_f,
             ini_date_p = id_p,
             lst_date_p = ld_p,
             mx = dts / exposure,
             bsn_mx = bsn / exposure,
             lp_mx = lp / exposure,
             up_mx = up / exposure)
    
    
    # preparing metadata
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # # names for plot and csv files
    # file_name <- paste0("sex_", str_flatten(sexes, "_"),
    #                     "_age_", str_flatten(ages, "_"),
    #                     "_model_", paste0(sec_trend_comp, "_", seasonal_comp),
    #                     "_weeks_exc_", exc_wks_ba_peak,
    #                     "_outliers_exc_", paste0(exc_outlrs, "_at_", outlrs_thld),
    #                     "_pred_invals_", pred_invals)
    # 
    # 
    # plot_title <- paste0("sex_", str_flatten(sexes, "_"),
    #                      "_age_", str_flatten(ages, "_"),
    #                      ", model: ", paste0(sec_trend_comp, "_", seasonal_comp),
    #                      ", weeks exc: ", exc_wks_ba_peak,
    #                      ", outliers exc: ", paste0(exc_outlrs, "_at_", outlrs_thld),
    #                      ", pred: ", pred_invals)
    # 
    # plot_sub_title <- paste0("fitted ", id_f, " to ", ld_f,
    #                          ", predicted ", id_p, " to ", ld_p)
    
    # meta <<-
    #   list(sxs_dim = length(unique(db_out$sex)),
    #        ags_dim = length(unique(db_out$age)),
    #        per_dim = length(unique(db_out$date)),
    #        plot_title = plot_title,
    #        plot_sub_title = plot_sub_title,
    #        file_name = file_name)
    
    return(db_out)
    
  }

epiyear <- function(x){
  # x <- ymd("2010-01-09")
  dn <- 1 + (wday(x) + 6) %% 7
  nth <- x + days(4 - dn)
  year(nth)
}
