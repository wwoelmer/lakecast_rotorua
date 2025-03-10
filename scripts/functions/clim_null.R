# function to calculate the climatology null for a given date


clim_null <- function(obs_file, # file path for observation csv
                      fcast_start_date, # date that the forecast will be made on
                      fcast_variable, # vector of forecast variables (names must match what is in obs_file)
                      fcast_depths, # vector of depths to include in teh forecast
                      horizon, # forecast horizon or number of days predicted into the future
                      n_historical_obs = 5 # the number of historical observation to show on plot
                      ){
  library(RColorBrewer)
  library(tidyverse)
  
  dates <- seq.Date(as.Date(fcast_start_date) - n_historical_obs, 
                    as.Date(fcast_start_date) + horizon, 
                    by = 'day')
  
  dat <- read.csv(obs_file) %>% 
    filter(variable %in% fcast_variable,
           as.Date(datetime) < max(dates))
  
  if(length(fcast_depths) > 0){
    dat <- dat %>% 
      filter(depth %in% fcast_depths)
  }
  
  # check that the filtering did not remove all data because of depth mismatch
  if(is.null(dat$depth)){
    print('no data available at desired forecast depth')
    return(NULL)
  }
  
  dat_daily <- dat %>% 
    mutate(date = as.Date(datetime)) %>% 
    group_by(date, variable, depth) %>% 
    summarise(daily_mean = mean(value, na.rm = TRUE))
  
  null <- dat_daily %>% 
    filter(ifelse(variable=='Water_Temp', daily_mean > 0, TRUE)) %>% # remove weird temp observations
    mutate(doy = yday(date),
           year = year(date)) %>% 
    group_by(variable, doy, depth) %>% 
    mutate(mean = mean(daily_mean, na.rm = TRUE),
           sd = sd(daily_mean, na.rm = TRUE),
           n = n()) %>% 
    filter(n > 2) %>% # requireds more than two observations to calculate a mean/sd
    distinct(variable, depth, doy, .keep_all = TRUE) %>% 
    dplyr::select(doy, variable, depth, mean, sd, n) 
    
  if(!is.null(dat$lake)){
    title <- paste0('Climatology forecasts for every day of the year up to ', fcast_start_date, 'at ', dat$lake)
    
  }else{
    title <- paste0('Climatology forecasts for every day of the year up to ', fcast_start_date)
  }
  
  p1 <- null %>% 
    ggplot(aes(x = doy, y = mean, color = as.factor(depth))) + 
    geom_point() +
    geom_ribbon(aes(ymin = mean-sd, ymax = mean + sd, alpha = 0.3, fill = as.factor(depth))) +
    facet_wrap(~variable, scales = 'free') +
    theme_bw() +
    theme(text = element_text(size = 16)) +
    ylab('Climatology Estimate') +
    xlab('Day of Year') +
    ggtitle(title)
  
  
  f_cols <- c('fcast_start_date', 
              'fcast_date', 
              'horizon')
  
  fcast <- data.frame(matrix(NA, nrow = horizon + n_historical_obs + 1, ncol = length(f_cols)))
  colnames(fcast) <- f_cols
  fcast$fcast_start_date <- fcast_start_date
  
  fcast <- fcast %>% 
    mutate(fcast_date = dates,
           doy = lubridate::yday(fcast_date))
  fcast$horizon <- seq(0 - n_historical_obs, horizon, by = 1)

  
  fcast2 <- left_join(fcast, null)
  fcast2 <- fcast2 %>% 
    filter(!is.na(variable))
  
  obs <- dat_daily %>% 
    filter(date %in% dates) %>% 
    rename(fcast_date = date,
           obs = daily_mean) %>% 
    mutate(depth = ifelse(depth==20.5, 20, depth))
  
  
  fcast2 <- fcast2 %>% 
    filter(horizon > -1) %>% 
    mutate(depth = ifelse(depth==20.5, 20, depth))
  
  col_no <- length(unique(fcast2$depth))
  col_pal <- colorRampPalette(brewer.pal(9, "RdYlBu"))(col_no)
  
  if(unique(fcast2$variable %in% c('Water_Temp', 'DO_sat', 'Chloro'))){
    fcast2$variable <- factor(fcast2$variable, levels = c('Water_Temp', 'DO_sat', 'Chloro'))
  }
  
  p2 <- ggplot(fcast2, aes(x = as.Date(fcast_date), y = mean, color = as.factor(depth))) +
          geom_point() +
          geom_ribbon(aes(ymin = mean-sd, ymax = mean + sd, alpha = 1, fill = as.factor(depth))) +
          geom_point(data = obs, size = 3, aes(x = as.Date(fcast_date), y = obs, shape = as.factor(depth), color = 'observations')) +
          facet_wrap(~fct_rev(variable), scales = 'free') +
          scale_color_manual(values = c(col_pal, 'black')) +
          scale_fill_manual(values = c(col_pal, 'black')) +
          theme_bw() +
          geom_vline(xintercept = as.Date(fcast_start_date)) +
          theme(text = element_text(size = 16)) +
          ggtitle(paste0('Climatology Forecast for ', fcast2$fcast_start_date[1])) +
          xlab('Date') +
          ylab('Forecast value') +
          labs(fill = 'Forecast Depth',
               shape = "Observations Depth") +
          guides(color = 'none',
                 alpha = 'none')
          
  
  # now score the forecasts and save output      
  score_df <- left_join(fcast2, obs, by = c('fcast_date', 'variable', 'depth'))
  score_df <- na.omit(score_df)
  score_df <- score_df %>% 
    group_by(fcast_date, variable, depth, horizon) %>% 
    mutate(crps = scoringRules::crps(y = obs, family = 'normal',
                               mean = mean, sd = sd))
  
  p3 <- ggplot(score_df, aes(x = as.Date(fcast_date), y = crps, color = as.factor(depth))) +
          geom_point(size = 3) +
          facet_wrap(~variable, scales = 'free') +
          scale_color_manual(values = c(col_pal, 'black')) +
          scale_fill_manual(values = c(col_pal, 'black')) +
          theme_bw() +
          geom_vline(xintercept = as.Date(fcast_start_date)) +
          theme(text = element_text(size = 16)) +
          ggtitle(paste0('Climatology Scores for ', fcast2$fcast_start_date[1])) +
          xlab('Date') +
          ylab('CRPS') +
          labs(color = 'Depth')  
  
  write.csv(score_df, paste0('./output/forecasts/climatology_scores_', fcast_start_date, '_.csv'), row.names = F)
  return(list(p1 = p1, p2 = p2, p3 = p3))
}



