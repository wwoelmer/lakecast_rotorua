
library(tidyverse)
library(lubridate)
library(fable)
library(scoringRules)
library(arrow)

options(dplyr.summarise.inform = FALSE)

##### 1. Random walk function from FO ####
forecast.RW  <- function(start, h_weeks = 4) { # horizon is four weeks
  
  # Work out when the forecast should start
  forecast_starts <- targets %>%
    group_by(Site) %>% 
    dplyr::filter(!is.na(observed), date < start)
  forecast_starts
  forecast_starts$date <- as.Date(forecast_starts$date)
  
  if (nrow(forecast_starts) !=0) {
    forecast_starts <- forecast_starts %>% 
      # Start the day after the most recent non-NA value
      dplyr::summarise(start_date = as.Date(max(date) + lubridate::days(1))) %>% # Date
      dplyr::mutate(h = h_weeks) %>% # Horizon value
      dplyr::ungroup()
    
    # Generate the RW model
    RW_model <- targets %>%
      group_by(Site) %>% 
      mutate(index_seq = seq_along(paste0(hydroyear, hydroweek))) %>% 
      dplyr::filter(date < start) %>%
      tsibble::as_tsibble(key = c('Site'), index = 'index_seq') %>%
      dplyr::filter(date < forecast_starts$start_date)  %>%
      fabletools::model(RW = fable::RW(observed))
    
    # Generate the forecast
    RW_forecast <- RW_model %>%
      fabletools::generate(h = h_weeks,
                           bootstrap = T,
                           times = 200) %>%
      rename(model_id = .model,
             predicted = .sim,
             ensemble = .rep) %>%
      as_tibble() %>% 
      mutate(start_time = start) %>% 
      group_by(Site, ensemble) %>% 
      mutate(forecast_date = start + weeks(seq_len(n())))
    
    
    message('RW forecast for ', start, ' at ', paste(unique(forecast_starts$Site), collapse = ", "))
    return(RW_forecast)
  }  else {
    message('RW forecast not run for ', start, ' at ', paste(unique(forecast_starts$Site), collapse = ", "))
  }
}

