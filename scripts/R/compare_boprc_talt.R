# compare BOPRC and TALT data

boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-12.csv')
talt <- read.csv('./data/talt_cyano/talt_cyano_formatted_2025-05-20.csv')
talt$Site <- trimws(talt$Site)

talt_long <- talt %>% 
  pivot_longer(FQ_PH:chl_RFU, names_to = 'variable', values_to = 'value')

boprc <- boprc %>% 
  filter(SampleDate >= min(talt$Date))

ggplot(boprc, aes(x = as.Date(SampleDate), y = TotalBioVolume, color = genus)) +
  geom_point() +
  facet_wrap(~Site)

boprc_grouped <- boprc %>% 
  group_by(SampleDate, Site) %>% 
  summarise(sum_biovolume = sum(TotalBioVolume))

ggplot(boprc_grouped, aes(x = as.Date(SampleDate), y = sum_biovolume)) +
  geom_point(size = 2) +
  facet_wrap(~Site) +
  theme_bw()

# change the names of sites so they will merge with TALT sites
boprc_grouped <- boprc_grouped %>% 
  mutate(Site = ifelse(Site=='Hamurana', 'Hamurana (bridge)', Site),
         Site = ifelse(Site=='Holdens Bay', "Hannah's Bay", Site)) %>% 
  rename(Date = SampleDate)

df_combo <- left_join(boprc_grouped, talt)

ggplot(df_combo, aes(x = FQ_chl, y = sum_biovolume)) +
  geom_point() +
  facet_wrap(~Site, scales = 'free')

ggplot(df_combo, aes(x = as.Date(Date), y = FQ_chl, color = 'TALT')) +
  geom_point() +
  geom_point(aes(y = sum_biovolume*50, color = 'BOPRC'), size = 3) +
  facet_wrap(~Site, scales = 'free') +
  scale_y_continuous(sec.axis = sec_axis(~./50))

ggplot(df_combo, aes(x = as.Date(Date), y = FQ_PH, color = 'TALT')) +
  geom_point() +
  geom_point(aes(y = sum_biovolume, color = 'BOPRC'), size = 3) +
  facet_wrap(~Site, scales = 'free')# +
  scale_y_continuous(sec.axis = sec_axis(~./50))
  
  ggplot(df_combo, aes(x = as.Date(Date), y = chl_RFU, color = 'TALT')) +
    geom_point() +
    geom_point(aes(y = sum_biovolume, color = 'BOPRC'), size = 3) +
    facet_wrap(~Site, scales = 'free')# +
  scale_y_continuous(sec.axis = sec_axis(~./50))

df_long <- left_join(boprc_grouped, talt_long)

ggplot(df_long, aes(x = sum_biovolume, y = value, color = Site))+
  geom_point() +
  facet_wrap(~variable, scales = 'free')
