# forecast with TALT data
library(tidyverse)

# read in targets
targets <- read.csv('./data/biovolume_targets.csv')
targets$date <- as.Date(targets$date)
targets <- targets %>% 
  arrange(date, Site) %>% 
  filter(!is.na(date))

################################################################################
# read in TALT shoreline data

talt <- read.csv('./data/talt_cyano/talt_cyano_formatted_2025-01-20_2025-05-28.csv')
talt <- talt %>% 
  select(Date, Site, Rep, FQ_PH, FQ_chl, chl_RFU) %>% 
  rename(date = Date)
talt$date <- as.Date(talt$date)

# calculate mean and sd over the reps
talt <- talt %>% 
  group_by(Site, date) %>% 
  summarise(mean_PH = mean(FQ_PH, na.rm = TRUE),
         sd_PH = sd(FQ_PH, na.rm = TRUE),
         mean_CHL = mean(FQ_chl, na.rm = TRUE),
         sd_CHL = sd(FQ_chl, na.rm = TRUE),
         mean_chl_EXO = mean(chl_RFU, na.rm = TRUE),
         sd_chl_EXO = sd(chl_RFU, na.rm = TRUE))

# rename the two sites to match with BOPRC
talt <- talt %>% 
  mutate(Site = ifelse(Site=='Hamurana (bridge)', 'Hamurana', Site),
         Site = ifelse(Site=="Hannah's Bay", 'Holdens Bay',  Site)) 

# add a match date column so that TALT data can be attributed to boprc targets between sampling
talt <- talt %>% 
  rename(talt_date = date) %>% 
  group_by(Site) %>% 
  mutate(date = targets$date[findInterval(talt_date, targets$date)])


################################################################################
# combine the dataframes
talt_bv <- left_join(targets, talt)

# remove days from before TALT sampling started
talt_bv <- talt_bv %>% 
  filter(date > min(talt$date))

hist(talt_bv$mean_PH)
quantile(talt_bv$mean_PH, probs = 0.75)

talt_bv %>% 
  filter(mean_PH > .1) %>% 
  ggplot(aes(x = log(sum_biovolume), y = log(mean_PH))) +
  geom_point() +
  facet_wrap(~Site)

ggplot(talt_bv, aes(x = log(sum_biovolume), y = log(mean_chl_EXO))) +
  geom_point() +
  facet_wrap(~Site) +
  geom_smooth()

ggplot(talt_bv, aes(x = date, y = sum_biovolume)) +
  geom_point() +
  facet_wrap(~Site) 

talt_bv %>% 
  filter(mean_PH > .1) %>% 
  ggplot(aes(x = log(sum_biovolume), y = log(sd_PH))) +
  geom_point() +
  facet_wrap(~Site)

talt_bv %>% 
  filter(mean_PH > .1) %>% 
  ggplot(aes(x = log(sum_biovolume), y = log(mean_CHL))) +
  geom_point() +
  facet_wrap(~Site)

talt_bv %>% 
  filter(mean_PH > .1) %>% 
  ggplot(aes(x = log(sum_biovolume), y = log(sd_CHL))) +
  geom_point() +
  facet_wrap(~Site)

talt_bv %>% 
  filter(mean_PH > .1) %>% 
  ggplot(aes(x = log(sum_biovolume), y = log(mean_PH/mean_CHL))) +
  geom_point() +
  facet_wrap(~Site) +
  geom_smooth(method = 'lm')

# take the mean of samples taken over the precious week
talt_bv_summ <- talt_bv %>% 
  group_by(Site, date, log_biovolume) %>% 
  summarise(mean_PH= mean(mean_PH),
         mean_CHL = mean(mean_CHL),
         sd_PH = sd(sd_PH),
         sd_CHL = sd(sd_CHL)) %>% 
  mutate(log_mean_PH = log(mean_PH),
         log_mean_CHL = log(mean_CHL)) %>% 
  filter(is.finite(log_mean_CHL))

fit <- lm(log_biovolume ~ log(mean_chl_EXO),
          data = talt_bv)
summary(fit)

library(mgcv)
fitgam <- gam(log_biovolume ~ s(log_mean_PH), data = talt_bv_summ)
summary(fitgam)

# log(sd_PH) + log(sd_CHL) +  log(mean_CHL) 
# model bv_t+1 = mean(talt_phyco_t) + sd(talt_phyco_t) + talt_phyco_t/talt_chl_t

# do first using data from same site

# then do three neighboring sites

# then do all sites?