# produce climatology forecasts at BOPRC sites from long-term records
# trigger to run script: new observations added to boprc_cyano_2015-01-07_DATE.csv
# OR new file added in that folder with name prefix of boprc_cyano_**
library(tidyverse)

fcast_year <- 2025 # what year are you forecasting, this is so we can subset to before this year

########## read in biovolume data
boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv')%>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume)) %>% 
  group_by(Site, SampleDate) %>% 
  summarise(sum_biovolume = sum(PotentiallyToxicBioVolume)) %>% # sum the biovolume on a given site and day (following NERM Cyanobacteria protocol 2024, alert levels are based on the combined total of all cyanobacteria)
  mutate(doy = yday(SampleDate), # add week and day of year
         year = year(SampleDate),
         week = week(SampleDate))

ggplot(boprc, aes(x = as.Date(SampleDate), y = sum_biovolume, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~Site) 

ggplot(boprc, aes(x = sum_biovolume)) +
  geom_histogram()

ggplot(boprc, aes(x = log(sum_biovolume))) +
  geom_histogram()

######### log-transform the data for normality
#calculate mean and sd of potentially toxic biovolume for each week and site
# first log transform the data because it is not normally distributed, then take mean/sd
min_val <- min(boprc$sum_biovolume[boprc$sum_biovolume > 0])

boprc <- boprc %>% 
  mutate(log_biovolume = log(sum_biovolume + min_val)) 

ggplot(boprc, aes(x = log_biovolume)) +
  geom_histogram()

########### calculate mean and std for each week/site
climatology <- boprc %>% 
  filter(year < fcast_year) %>% 
  group_by(week, Site) %>% 
  summarise(mu_log = mean(log_biovolume),
            sd_log = sd(log_biovolume),
            n_obs = n()) %>% 
  filter(n_obs > 2) # only consider weeks with more than 2 obs

ggplot(climatology, aes(x = week, y = mu_log)) +
  geom_point() +
  geom_errorbar(aes(ymin = mu_log - sd_log, ymax = mu_log + sd_log), width = 0.5) +
  facet_wrap(~Site)

climatology <- climatology %>% 
  group_by(Site, week) %>% 
  mutate(lower_CI_pred = exp(qnorm(c(0.05, 0.95), mean = mu_log, sd = sd_log)[1]- min_val),
         upper_CI_pred = exp(qnorm(c(0.05, 0.95), mean = mu_log, sd = sd_log)[2] - min_val),
         mean_pred = exp(qnorm(c(0.5), mean = mu_log, sd = sd_log)- min_val),
         sd_pred = exp(sd_log - min_val))

ggplot(climatology, aes(x = week, y = mean_pred)) +
  geom_point() +
  geom_ribbon(aes(ymin = lower_CI_pred, ymax = upper_CI_pred), alpha = 0.4) +
  facet_wrap(~Site) +
  theme_bw() +
  ylab('Biovolume with 95% CI')

########### create an ensemble distribution for each week/site based on the observed mean and std
set.seed(123)
pred <- climatology %>% 
  group_by(Site, week) %>% 
  mutate(ens_pred = list(rlnorm(1000, meanlog = mu_log, sdlog = sd_log)))

########## calculate the mean and confidence intervals for the simulated distribution
pred_long <- pred %>%
  select(Site, week, ens_pred) %>%
  unnest(ens_pred) %>% 
  group_by(Site, week) %>% 
  summarise(mean_ens = mean(ens_pred),
            p05 = quantile(ens_pred, 0.05),
            p95 = quantile(ens_pred, 0.95),)

ggplot(pred_long, aes(x = week, y = mean_ens)) +
  geom_point() +
  geom_ribbon(aes(ymin = p05, ymax = p95, alpha = 0.4)) +
  facet_wrap(~ Site, scales = 'free') +
  theme_bw()

########## calculate the probability of each warning
warnings <- pred %>% 
  select(Site, week, ens_pred) %>%
  unnest(ens_pred) %>% 
  # classify each ensemble member as falling into which warning level
  mutate(warnings = case_when(ens_pred < 0.5 ~ "Green",
                              ens_pred >= 0.5 & ens_pred < 10 ~ 'Orange',
                              ens_pred >= 10 ~ 'Red')) %>% 
  group_by(Site, week, warnings) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Site, week) %>%
  # calculate the percent of ensemble members in each warning level
  mutate(prob = n / sum(n)) %>%
  select(Site, week, warnings, prob)

ggplot(warnings, aes(x = week, y = prob, fill = warnings)) +
  geom_col(position = 'stack') +
  facet_wrap(~Site) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  theme_bw()

# just check that the probabilities sum to 100
daily_summary <- warnings %>% 
  group_by(Site, week) %>% 
  summarise(total_prob = sum(prob, na.rm = TRUE))

clim_out <- climatology %>% 
  select(Site, week, mean_pred, sd_pred) %>% 
  left_join(warnings)


write.csv(clim_out, './forecasts/climatology_values_probabilities.csv', row.names = FALSE)
#################################################################################
# produce spatial persistence forecasts at BOPRC using TALT sites

# first need to create regression between cyanofluor phyco and potentially toxic biovolume
# see Georgia's paper for reference on this: https://www.sciencedirect.com/science/article/pii/S1568988320301487
# do this for each site using data collected by TALT so far
# so far, this is just 16 observations, but it will grow
# and then we have 2 more obs per week to inform the spatial persistence

#################################################################################
# produce temporal persistence forecasts at BOPRC sites using buoy data