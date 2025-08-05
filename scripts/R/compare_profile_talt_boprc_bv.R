# compare talt data and boprc biovolume data

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

bop_biovol <- bop_biovol %>% 
  mutate(week = week(date)) %>% 
  rename(bop_date = date)

#################################################################################
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

talt_long$date <- as.Date(talt_long$date)

# add week column for comparison with boprc data
talt_long <- talt_long %>% 
  mutate(week = week(date)) %>% 
  rename(talt_date = date)

# rename hamurana and Hannah's Bay to match with BOPRC data
talt_long <- talt_long %>% 
  mutate(site = recode(site,
                       "Hamurana (bridge)" = "Hamurana",
                       "Hannah's Bay" = "Holdens Bay"))

################################################################################
# combine the dataframes
bv_talt <- left_join(talt_long, bop_biovol)

# subset to dates after TALT sampling began
bv_talt <- bv_talt %>% 
  filter(bop_date > min(talt_long$talt_date))

ggplot(bv_talt, aes(x = week, y = toxic_bv)) +
  geom_point() +
  facet_wrap(~site)

ggplot(bv_talt, aes(x = toxic_bv, y = talt_value, color = talt_var)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(talt_var~site, scales = 'free', nrow = 5) +
  theme_bw() + 
  xlab('Log of Toxic Biovolume') +
  ylab('Log of TALT-monitored algal metrics') +
  ggtitle('Relationship between TALT-monitored data and BOPRC Toxic BV')
