# get climatology forecasts output
library(tidyverse)
source('./scripts/functions/clim_null.R')


obs_file <- './data/buoy/rotorua_fixed_sensor_data_2008-01-01_2022-05-04.csv'
fcast_start_date <- '2020-01-01'
fcast_variable = c('Water_Temp', 'DO_sat', 'Chloro')
fcast_depths = c(1.0, 5.0, 10.5)
horizon = 365
clim_null(obs_file = obs_file, 
          fcast_start_date = fcast_start_date,
          fcast_variable = fcast_variable,
          fcast_depths = fcast_depths,
          horizon = horizon)


obs_file <- './data/buoy/rotorua_fixed_sensor_data_2008-01-01_2022-05-04.csv'
fcast_start_date <- '2022-01-02'
fcast_variable = c('Water_Temp', 'DO_sat', 'Chloro')
fcast_depths = c(1)
horizon = 14
figs <- clim_null(obs_file = obs_file, 
          fcast_start_date = fcast_start_date,
          fcast_variable = fcast_variable,
          fcast_depths = fcast_depths,
          horizon = horizon)


figs$p2

t <- read.csv('./data/forecasts/climatology_scores_2021-01-02_.csv')
