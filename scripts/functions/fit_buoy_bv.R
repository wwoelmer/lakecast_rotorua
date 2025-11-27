# test the function
#data <- buoy_bv
#fcast_start <- '2024-11-25'
#horizon <- 1
#fcast_site <- 'Hamurana'
#lag <- 1

library(tidyverse)

fit_buoy_bv <- function(data, # dataset with bv and lagged buoy int
                        fcast_start, # start date of forecast (e.g., present day)
                        horizon = 1, # how many weeks ahead you want to predict
                        variable = 'phyco', # which variable, phyco or chl
                        fcast_site, # which site to forecast
                        lag = 1, # which lag to make the forecast with, default is 1 day before bv measurement
                        min_val = 0.001 # minimum amount to add before logging
){ 
  
  # subset data 
  df_train <- data %>% 
    filter(date < fcast_start,
           Site==fcast_site)
  
  # select the variable, and rename it as generic
  driver_col <- c('lag1_phyco', 'lag1_chl') #paste0('lag', lag, '_', variable)
  df_train <- df_train %>% 
    select(date, Site, sum_biovolume, all_of(driver_col)) #%>% 
  #rename(driver = all_of(driver_col))
  
  #log it
  df_train <- df_train %>% 
    mutate(log_biovolume = log(sum_biovolume + min_val),
           log_lag1_phyco = log(lag1_phyco + min_val),
           log_lag1_chl = log(lag1_chl + min_val))
  #log_driver = log(driver + min_val))
  
  df_train <- df_train %>% 
    filter(!is.na(log_biovolume))
  
  ggplot(df_train, aes(x = as.Date(date), y = log_biovolume, color = 'biovol')) +
    geom_point() +
    geom_point(aes(x = date, y = log_lag1_chl, color = 'buoy')) +
    facet_wrap(~Site)
  
  # fit the model on all training data
  fit <- lm(log_biovolume ~ log_lag1_phyco + log_lag1_chl, data = df_train) #lm(as.formula(paste("log_biovolume ~ log_driver")), data = df_train)
  summary(fit)
  
  newdata <- data %>% 
    filter(date==fcast_start,
           Site==fcast_site) %>% 
    select(date, sum_biovolume, lag1_phyco, lag1_chl) %>% 
    mutate(log_biovolume = log(sum_biovolume + min_val),
           log_lag1_phyco = log(lag1_phyco + min_val),
           log_lag1_chl = log(lag1_chl + min_val))
  
  
  # run the model with new data
  pred <- predict(fit, newdata = newdata, interval = 'prediction', 
                  level = 0.95)
  
  # get observations
  obs <- data %>% 
    filter(date==fcast_start,
           Site==fcast_site) %>% 
    pull(sum_biovolume)
  
  # format output  
  tibble(date = fcast_start,
         Site = fcast_site,
         days_lag = lag,
         buoy_phyco_lag = newdata$lag1_phyco,
         buoy_chl_lag = newdata$lag1_chl,
         pred_bv_native = exp(pred[1] - min_val), # back-transform
         pred_lower = exp(pred[2] - min_val),
         pred_upper = exp(pred[3] - min_val),
         observed_bv = obs)  
  
}

