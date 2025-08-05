# compare buoy to biovolume

library(tidyverse)

################################################################################
## read in biovolume data from BOPRC
bop_biovol <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv')
bop_biovol <- bop_biovol %>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume))

# clean up some column names
bop_biovol <- bop_biovol %>% 
  select(-Location) %>% 
  rename(date = SampleDate,
         site = Site)

bop_biovol$date <- as.Date(bop_biovol$date)

# sum biovolume on each site and date
bop_biovol <- bop_biovol %>% 
  group_by(site, date) %>% 
  summarise(toxic_bv = sum(PotentiallyToxicBioVolume))

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

buoy_daily %>% 
  filter(variable=='FlPhyc') %>% 
  ggplot(aes(x = as.Date(date), y = value, color = DptSns)) +
  geom_point()

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
buoy_bv <- left_join(bop_biovol, buoy_int)

# remove days from before profiling buoy went out
buoy_bv <- buoy_bv %>% 
  filter(date > min(buoy_int$date),
         !is.na(variable))

ggplot(buoy_bv, aes(x = log(toxic_bv), y = log(var_integrated), color = variable)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable + site, scales = 'free', nrow = 2) +
  ylab('Log of Depth Integrated Total (RFU)') +
  xlab('Log of Toxic Biovolume (mm3/L)') +
  theme_bw() +
  scale_color_manual(values = c('darkgreen', 'cyan3'))

ggplot(buoy_bv, aes(date, y = toxic_bv)) +
  geom_point() +
  facet_wrap(~site, scales = 'free')
