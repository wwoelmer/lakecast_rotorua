library(tidyverse)
library(ggpubr)

# read in targets
targets <- read.csv('./data/biovolume_targets.csv')
targets$date <- as.Date(targets$date)

################################################################################
# read in profiling buoy data
buoy <- read.csv('./data/buoy/rotorua_profiles_latest.csv')
buoy_daily <- buoy %>% 
  pivot_longer(TmpWtr:FlPhyc, names_to = 'variable', values_to = 'value') %>% 
  mutate(date = as.Date(DateTime)) %>% 
  group_by(date, variable, DptSns) %>% 
  summarise(value = mean(value, na.rm = TRUE))

buoy_daily <- buoy_daily %>% 
  filter(variable %in% c('FlChlr', 'FlPhyc'))

# calculate depth-integrated chl and phyco on each day across water col
# For var_integrated, what you're doing is adding up each pair of neighboring depths, 
# then dividing by two to get the average value at that depth interval, 
# then you multiply by the distance (m) between each depth to standardize, 
# and add it all up for the integration

buoy_int <- buoy_daily %>% 
  filter(!is.na(value) & !is.na(DptSns)) %>%
  arrange(DptSns) %>%
  group_by(date, variable) %>% 
  summarise(var_integrated = sum(diff(DptSns, na.rm = TRUE) * (head(value, -1) + tail(value, -1)) / 2),
            var_int_avg = var_integrated/max(DptSns)) 

# Var_integrated is the sum of the depth integrated variable, so for chl fluorescence for example, it would be in units of RFU-m
# Var_int_avg  is the depth integrated variable, but standardized by the maximum depth, so it returns to units of RFU

ggplot(buoy_int, aes(x = as.Date(date), y = var_int_avg)) +
  geom_point() +
  facet_wrap(~variable, scale = 'free')

# make wide format
buoy_wide <- buoy_int %>% 
  select(date, variable, var_int_avg) %>% 
  pivot_wider(names_from = 'variable', values_from = 'var_int_avg') %>% 
  rename('phyco_RFU_int_avg' = 'FlPhyc',
         'chl_RFU_int_avg' = 'FlChlr')

# create lags up to one week
buoy_wide <- buoy_wide %>% 
  arrange(date) %>% 
  ungroup() %>% 
  mutate(lag1_phyco = lag(phyco_RFU_int_avg, n = 1L),
         lag2_phyco = lag(phyco_RFU_int_avg, 2),
         lag3_phyco = lag(phyco_RFU_int_avg, 3),
         lag4_phyco = lag(phyco_RFU_int_avg, 4),
         lag5_phyco = lag(phyco_RFU_int_avg, 5),
         lag6_phyco = lag(phyco_RFU_int_avg, 6),
         lag7_phyco = lag(phyco_RFU_int_avg, 7)) %>% 
  mutate(lag1_chl = lag(chl_RFU_int_avg, 1),
         lag2_chl = lag(chl_RFU_int_avg, 2),
         lag3_chl = lag(chl_RFU_int_avg, 3),
         lag4_chl = lag(chl_RFU_int_avg, 4),
         lag5_chl = lag(chl_RFU_int_avg, 5),
         lag6_chl = lag(chl_RFU_int_avg, 6),
         lag7_chl = lag(chl_RFU_int_avg, 7))

a <- buoy_wide %>% 
  filter(date > '2024-11-30') %>% 
  ggplot(aes(x = as.Date(date), y = phyco_RFU_int_avg, color = 'buoy phyco RFU')) +
  xlim(as.Date('2024-11-30'), as.Date('2025-05-31')) + 
  ylim(0, 20) +
  geom_point() +
  geom_line() +
  geom_point(data = targets[targets$date > as.Date('2021-11-30'),], aes(x = as.Date(date), y = sum_biovolume, color = Site)) +
  geom_line(data = targets[targets$date > as.Date('2021-11-30'),], aes(x = as.Date(date), y = sum_biovolume, color = Site)) +
  theme_bw()
  
a
################################################################################
# combine the dataframes
buoy_bv <- left_join(targets, buoy_wide)

# remove days from before profiling buoy went out
buoy_bv <- buoy_bv %>% 
  filter(date > min(buoy_int$date))

ggplot(buoy_bv, aes(x = log(sum_biovolume), y = log(phyco_RFU_int_avg))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~Site) +
  ylab('Log of Depth Averaged Fluorescence (RFU)') +
  xlab('Log of Toxic Biovolume (mm3/L)') +
  theme_bw() 

# clean up some columns 
buoy_bv <- buoy_bv %>% 
  select(Site, date, sum_biovolume, chl_RFU_int_avg, phyco_RFU_int_avg, lag1_phyco:lag7_chl)

# test the function
data <- buoy_bv
fcast_start <- '2025-04-02'
horizon <- 1
fcast_site <- 'Hamurana'
lag <- 1

#### function stuff

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
      driver_col <- c('lag1_phyco', 'lag1_chl',
                      'lag6_phyco', 'lag6_chl') #paste0('lag', lag, '_', variable)
      df_train <- df_train %>% 
        select(date, Site, sum_biovolume, all_of(driver_col)) #%>% 
        #rename(driver = all_of(driver_col))
      
      #log it
      df_train <- df_train %>% 
        mutate(log_biovolume = log(sum_biovolume + min_val),
               log_lag1_phyco = log(lag1_phyco + min_val),
               log_lag1_chl = log(lag1_chl + min_val),
               log_lag6_phyco = log(lag6_phyco + min_val),
               log_lag6_chl = log(lag6_chl + min_val))
               #log_driver = log(driver + min_val))
      
      df_train <- df_train %>% 
        filter(!is.na(log_biovolume))
      
      ggplot(df_train, aes(x = as.Date(date), y = log_biovolume, color = 'biovol')) +
        geom_point() +
        geom_point(aes(x = date, y = log_lag1_chl, color = 'buoy')) +
        facet_wrap(~Site)
      
      # fit the model on all training data
      fit <- lm(log_biovolume ~ log_lag6_phyco + log_lag6_chl, data = df_train) #lm(as.formula(paste("log_biovolume ~ log_driver")), data = df_train)
      summary(fit)
      
      newdata <- data %>% 
        filter(date==fcast_start,
               Site==fcast_site) %>% 
        select(date, sum_biovolume, lag1_phyco, lag1_chl) %>% 
        mutate(log_biovolume = log(sum_biovolume + min_val),
               log_lag1_phyco = log(lag1_phyco + min_val),
               log_lag1_chl = log(lag1_chl + min_val))
      
      
      # run the model with new data
      pred <- predict(fit, newdata = newdata)
      
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
             pred_bv_native = exp(pred - 0.001), # back-transform
             observed_bv = obs)  
  
}


dates <- seq.Date(as.Date('2024-11-25'), as.Date('2025-05-21'), by = 'week')
dates <- unique(buoy_bv$date)
dates <- dates[dates > as.Date('2024-11-25')]

sites <- unique(buoy_bv$Site)
out <- data.frame()
for(i in 1:length(dates)){
  for(j in 1:length(sites)){
    print(dates[i])
    print(sites[j])
    temp <- fit_buoy_bv(data = buoy_bv,
                        fcast_start = dates[i],
                        horizon = 1,
                        lag = 1,
                        fcast_site = sites[j])
    out <- rbind(out, temp)
  }
}

ggplot(out, aes(x = date, y = pred_bv_native, color = 'pred')) +
  geom_point() +
  geom_line() +
  geom_point(aes(x = date, y = observed_bv, color = 'obs')) +
  geom_line(aes(x = date, y = observed_bv, color = 'obs')) +
  facet_wrap(~Site) +
  theme_bw()

out <- out %>% 
  group_by(Site, date) %>% 
  mutate(rmse = sqrt(mean((pred_bv_native - observed_bv)^2, na.rm = TRUE)))

ggplot(out, aes(x = date, y = rmse)) +
  geom_point() +
  facet_wrap(~Site)

write.csv(out, './output/buoy_persistence_forecast_scores.csv', row.names = FALSE)
