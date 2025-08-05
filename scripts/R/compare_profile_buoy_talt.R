# compare buoy to biovolume

library(tidyverse)

################################################################################
## read in TALT data
talt <- read.csv('./data/talt_cyano/talt_cyano_formatted_2025-01-20_2025-07-23.csv')
talt <- talt %>% 
  select(Date, Site, FQ_PH, FQ_chl, CF_PH, CF_chl, chl_RFU) %>% 
  rename(date = Date,
         site = Site,
         YSI_chl = chl_RFU)

talt_long <- talt %>% 
  pivot_longer(FQ_PH:YSI_chl, names_to = 'talt_var', values_to = 'talt_value')

talt$date <- as.Date(talt$date)

ggplot(talt_long, aes(x = as.Date(date), y = talt_value, color = talt_var)) +
  geom_point() +
  facet_wrap(~talt_var, scales = 'free') +
  theme_bw()

################################################################################
# read in profiling buoy data
buoy <- read.csv('./data/buoy/Rotorua_202202-202507_profiles.csv')
buoy_daily <- buoy %>% 
  pivot_longer(TmpWtr:FlPhyc, names_to = 'variable', values_to = 'value') %>% 
  mutate(date = as.Date(DateTime)) %>% 
  group_by(date, variable, DptSns) %>% 
  summarise(value = mean(value, na.rm = TRUE))

buoy_daily <- buoy_daily %>% 
  filter(variable %in% c('FlChlr', 'FlPhyc'))

# calculate depth-integrated chl and phyco on each day across water col
buoy_int <- buoy_daily %>% 
  filter(!is.na(value) & !is.na(DptSns)) %>%
  arrange(DptSns) %>%
  group_by(date, variable) %>% 
  summarise(var_integrated = sum(diff(DptSns, na.rm = TRUE) * (head(value, -1) + tail(value, -1)) / 2),
            var_int_avg = var_integrated/max(DptSns))

ggplot(buoy_int, aes(x = as.Date(date), y = var_integrated)) +
  geom_point() +
  facet_wrap(~variable, scale = 'free')

ggplot(buoy_int, aes(x = as.Date(date), y = var_int_avg)) +
  geom_point() +
  facet_wrap(~variable, scale = 'free')

################################################################################
# combine the dataframes
buoy_talt <- left_join(talt_long, buoy_int)

# remove days from before TALT sampling began
buoy_talt <- buoy_talt %>% 
  filter(date > min(talt_long$date),
         !is.na(variable))

# first look at just the four sites from long-term monitoring program
buoy_talt %>% 
  filter(variable=='FlChlr',
         site %in% c("Ohau Channel", "Ngongotaha", "Hannah's Bay", "Hamurana (bridge)")) %>% 
  ggplot(aes(x = log(var_integrated), y = log(talt_value), color = talt_var)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(talt_var~site, scales = 'free', nrow = 5) +
  theme_bw() +
  ggtitle('Relationship between buoy depth integrated chl and TALT sites')

buoy_talt %>% 
  filter(variable=='FlPhyc',
         site %in% c("Ohau Channel", "Ngongotaha", "Hannah's Bay", "Hamurana (bridge)")) %>% 
  ggplot(aes(x = log(var_integrated), y = log(talt_value), color = talt_var)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(talt_var~site, scales = 'free', nrow = 5) +
  theme_bw() +
  ggtitle('Relationship between buoy depth integrated phycocyanin and TALT sites')

buoy_talt %>% 
  filter(site %in% c("Ohau Channel", "Ngongotaha", "Hannah's Bay", "Hamurana (bridge)")) %>% 
  ggplot(aes(date, y = var_integrated, color = variable)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free', nrow = 2) +
  theme_bw()

buoy_talt %>% 
  filter(site %in% c("Ohau Channel", "Ngongotaha", "Hannah's Bay", "Hamurana (bridge)")) %>% 
  ggplot(aes(date, y = talt_value, color = talt_var)) +
  geom_point() +
  facet_wrap(talt_var~site, scales = 'free', nrow = 5) +
  theme_bw()
