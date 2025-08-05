# convert buoy phyco and chl RFU into biovolume
library(tidyverse)

# read in fixed sensor buoy data
buoy <- read.csv('./data/buoy/rotorua_fixed_sensor_data_2008-01-01_2022-05-04.csv')
buoy_daily <- buoy %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date, variable, depth) %>% 
  summarise(value = mean(value, na.rm = TRUE))

buoy_daily <- buoy_daily %>% 
  rename(value_fixed = value)
  
buoy_daily %>% 
  filter(variable=='Water_Temp', 
         value > 0 & value < 40) %>% 
  ggplot(aes(x = as.POSIXct(date), y = value, color = as.factor(depth))) +
  geom_point()

buoy_daily %>% 
  filter(variable=='Chloro') %>% 
  ggplot(aes(x = as.POSIXct(date), y = value, color = as.factor(depth))) +
  geom_point()

buoy_daily <- buoy_daily %>% 
  mutate(variable = recode(variable, 'Chloro' = 'FlChlr'))

# read in profiler buoy data
prof <- read.csv('./data/buoy/Rotorua_202202-202505_profiles.csv')
prof_long <- prof %>% 
  pivot_longer(TmpWtr:FlPhyc, names_to = 'variable', values_to = 'value')

prof_daily <- prof_long %>% 
  mutate(depth_rnd = floor(DptSns*2)/2,
         date = as.Date(DateTime)) %>% 
  group_by(depth_rnd, date, variable) %>% 
  summarise(value_profiler =  mean(value, na.rm = TRUE)) %>% 
  rename(depth = depth_rnd)

prof_daily <- prof_daily %>% 
  mutate(variable = recode(variable, 'DOpsat' = 'DO_sat'),
         variable = recode(variable, 'TmpWtr' = 'Water_Temp'))

# join together
buoy_both <- full_join(buoy_daily, prof_daily)  

overlap <- buoy_both %>% 
  filter(!is.na(value_fixed), 
         !is.na(value_profiler))

buoy_both %>% 
  filter(variable %in% c('DO_sat', 'FlChlr', 'Water_Temp')) %>% 
  ggplot(aes(x = value_fixed, y = value_profiler, color = as.factor(depth))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')

buoy_both %>% 
  filter(variable %in% c('FlChlr', 'FlPhyc')) %>% 
  ggplot(aes(x = as.Date(date), y = value, color = as.factor(depth))) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')
### NOTE: there is very little temporal overlap and the DO and chl fluorescence
### sensor on the fixed thermistor appear to be not working (only report 0)
### will focus on profiler data

prof_daily

# read in boprc cyano data
boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-12.csv')


