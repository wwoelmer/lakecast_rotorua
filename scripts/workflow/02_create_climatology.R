# produce climatology forecasts at BOPRC sites from long-term records
# trigger to run script: new observations added to boprc_cyano_2015-01-07_DATE.csv
# OR new file added in that folder with name prefix of boprc_cyano_**
library(tidyverse)

boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv')

boprc <- boprc %>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume))

# sum the biovolume on a given site and day (following NERM Cyanobacteria protocol 2024,
# alert levels are based on the combined total of all cyanobacteria)
boprc <- boprc %>% 
  group_by(Site, SampleDate) %>% 
  summarise(sum_biovolume = sum(PotentiallyToxicBioVolume))

ggplot(boprc, aes(x = sum_biovolume)) +
  geom_histogram()

ggplot(boprc, aes(x = log(sum_biovolume))) +
  geom_histogram()

# convert potentially toxic into warning levels based on guideline
# thresholds from BOPRC based on biovolume
boprc <- boprc %>% 
  mutate(warning = case_when(sum_biovolume < 0.5 ~ 'green',
                             sum_biovolume > 0.5 & sum_biovolume < 9.99 ~ 'orange',
                             sum_biovolume > 10 ~ 'red'))

ggplot(boprc, aes(x = as.Date(SampleDate), y = warning, color = warning)) +
  geom_point() +
  facet_wrap(~Site) +
  scale_color_manual(values = c('green', 'orange', 'red', 'gray'))

# add week and day of year
boprc <- boprc %>% 
  mutate(doy = yday(SampleDate),
         year = year(SampleDate),
         week = week(SampleDate))

ggplot(boprc, aes(x = week, y = warning, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~Site) 

ggplot(boprc, aes(x = week, y = sum_biovolume, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~Site) 

#calculate mean and sd of potentially toxic biovolume for each week and site
# first log transform the data because it is not normally distributed, then take mean/sd
min_val <- min(boprc$sum_biovolume[boprc$sum_biovolume > 0])

climatology <- boprc %>% 
  group_by(Site, week) %>% 
  mutate(mean_raw = mean(sum_biovolume),
         sd_raw = sd(sum_biovolume)) %>% 
  mutate(log_biovolume = log(sum_biovolume + min_val)) %>% 
  mutate(mean = mean(log_biovolume),
            sd = sd(log_biovolume, na.rm = TRUE)) %>% 
  distinct(Site, week, .keep_all = TRUE) %>% 
  select(Site, SampleDate, week, mean_raw, sd_raw, mean, sd)
         

ggplot(climatology, aes(x = week, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
  facet_wrap(~Site)

# now pull from a distribution for each doy, then convert into likelihood of red, orange, or green warning
pred <- climatology %>% 
  group_by(Site, SampleDate) %>% 
  mutate(ens_pred = list(rlnorm(1000, meanlog = mean, sdlog = sd)))

pred %>% 
  unnest() %>% 
  ggplot(aes(x = week, y = mean_raw)) +
  geom_point() +
  facet_wrap(~Site, scales = 'free')

warnings <- pred %>% 
  rowwise() %>% 
  mutate(green = mean(ens_pred < 0.5),
         orange = mean(ens_pred >=0.5 & ens_pred < 10),
         red = mean(ens_pred >= 10))

t2 <- pred %>% 
  rowwise() %>% 
  mutate(green = count(ens_pred < 0.5)/1000)

warnings_long <- warnings %>% 
  pivot_longer(green:red, values_to = 'probability', names_to = 'warning_level')

ggplot(warnings_long, aes(x = week, y = probability, fill = warning_level)) +
  geom_col(position = 'stack') +
  facet_wrap(~Site) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  theme_bw()

ggplot(warnings_long, aes(x = week, y = probability, group = warning_level, 
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
  ggplot(aes(x = week, y = probability)) +
  geom_line(size = 2) +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0.50) +
  theme_bw() +
  ylab('Probability of red warning')

# just check that the probabilities sum to 100
daily_summary <- warnings_long %>% 
  group_by(Site, week) %>% 
  summarise(total_prob = sum(probability, na.rm = TRUE))

warnings_save <- warnings_long %>% 
  select(-ens_pred)

write.csv(warnings_save, './forecasts/climatology_values_probabilities.csv', row.names = FALSE)
#################################################################################
# produce spatial persistence forecasts at BOPRC using TALT sites

# first need to create regression between cyanofluor phyco and potentially toxic biovolume
# see Georgia's paper for reference on this: https://www.sciencedirect.com/science/article/pii/S1568988320301487
# do this for each site using data collected by TALT so far
# so far, this is just 16 observations, but it will grow
# and then we have 2 more obs per week to inform the spatial persistence

#################################################################################
# produce temporal persistence forecasts at BOPRC sites using buoy data