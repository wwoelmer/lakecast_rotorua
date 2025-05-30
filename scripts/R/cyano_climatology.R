# produce climatology forecasts at BOPRC sites from long-term records
boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-12.csv')

boprc <- boprc %>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume))

ggplot(boprc, aes(x = log(PotentiallyToxicBioVolume))) +
  geom_histogram()

# convert potentially toxic into warning or no based on guidelines
# thresholds from BOPRC based on biovolume
boprc <- boprc %>% 
  mutate(warning = case_when(PotentiallyToxicBioVolume < 0.5 ~ 'green',
                             PotentiallyToxicBioVolume > 0.5 & PotentiallyToxicBioVolume < 9.99 ~ 'orange',
                             PotentiallyToxicBioVolume > 10 ~ 'red'))

ggplot(boprc, aes(x = as.Date(SampleDate), y = warning, color = warning)) +
  geom_point() +
  facet_wrap(~Site) +
  scale_color_manual(values = c('green', 'orange', 'red', 'gray'))

# calculate mean and sd of potentially toxic biovolume for each doy and site
boprc <- boprc %>% 
  mutate(doy = yday(SampleDate),
         year = year(SampleDate))

ggplot(boprc, aes(x = doy, y = warning, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~Site) 

ggplot(boprc, aes(x = doy, y = PotentiallyToxicBioVolume, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~Site) 

# first log transform the data because it is not normally distributed, then take mean/sd
min_val <- min(boprc$PotentiallyToxicBioVolume[boprc$PotentiallyToxicBioVolume > 0])

climatology <- boprc %>% 
  group_by(Site, doy) %>% 
  mutate(log_biovolume = log(PotentiallyToxicBioVolume + min_val)) %>% 
  summarise(mean = mean(log_biovolume),
            sd = sd(log_biovolume, na.rm = TRUE))
         

ggplot(climatology, aes(x = doy, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
  facet_wrap(~Site)

# now pull from a distribution for each doy, then convert into likelihood of red, orange, or green warning
pred <- climatology %>% 
  rowwise() %>% 
  mutate(ens_pred = list(rlnorm(1000, meanlog = mean, sdlog = sd)))

warnings <- pred %>% 
  rowwise() %>% 
  mutate(green = mean(ens_pred < 0.5),
         orange = mean(ens_pred >=0.5 & ens_pred < 10),
         red = mean(ens_pred >= 10))

warnings_long <- warnings %>% 
  pivot_longer(green:red, values_to = 'probability', names_to = 'warning_level')

ggplot(warnings_long, aes(x = doy, y = probability, fill = warning_level)) +
  geom_col(position = 'stack') +
  facet_wrap(~Site) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  theme_bw()

ggplot(warnings_long, aes(x = doy, y = probability, group = warning_level, 
                          color = warning_level)) +
  geom_line(size = 1) +
  facet_wrap(~Site) +
  scale_color_manual(values = c('green', 'orange', 'red')) +
  theme_bw()

ggplot(warnings_long, aes(x = probability, fill = Site)) +
  geom_histogram(position = 'dodge') +
  facet_wrap(~warning_level) +
  theme_bw()

warnings_long %>% 
  filter(warning_level=='red') %>% 
  ggplot(aes(x = doy, y = probability)) +
  geom_line(size = 2) +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0.50) +
  theme_bw() +
  ylab('Probability of red warning')

# just check that the probabilities sum to 100
daily_summary <- warnings_long %>% 
  group_by(Site, doy) %>% 
  summarise(total_prob = sum(probability, na.rm = TRUE))

#################################################################################
# produce spatial persistence forecasts at BOPRC using TALT sites

# first need to create regression between cyanofluor phyco and potentially toxic biovolume
# see Georgia's paper for reference on this: https://www.sciencedirect.com/science/article/pii/S1568988320301487
# do this for each site using data collected by TALT so far
# so far, this is just 16 observations, but it will grow
# and then we have 2 more obs per week to inform the spatial persistence

#################################################################################
# produce temporal persistence forecasts at BOPRC sites using buoy data