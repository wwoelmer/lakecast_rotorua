# score climatology forecasts

# read in climatology forecasts
clim <- read.csv('./forecasts/climatology_values_probabilities.csv')

# read in targets
targets <- read.csv('./data/biovolume_targets.csv')


# pull 2025 forecast dates
targets <- targets %>% 
  filter(date > as.Date("2024-12-31"))

# add week to merge with climatology
targets <- targets %>% 
  mutate(week = week(date)) %>% 
  select(Site, date, week, sum_biovolume)

clim_obs <- clim %>% 
  left_join(targets)

scores <- clim_obs %>%
  distinct(Site, date, mean_pred, sd_pred, sum_biovolume) %>% 
  group_by(Site, date) %>%
  summarise(crps = crps_lnorm(y = sum_biovolume, # in native units
                              mean =  log(mean_pred), # in log units
                              sd = log(sd_pred)), # in log units
            rmse = sqrt(mean((mean_pred - sum_biovolume)^2, na.rm = TRUE)) ) 


ggplot(scores, aes(x = as.Date(date), y = crps)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  ggtitle('Climatology scores')

ggplot(scores, aes(x = as.Date(date), y = rmse)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  ggtitle('Climatology scores')

scores %>% 
  group_by(Site) %>% 
  summarise(mean_crps = mean(crps, na.rm = TRUE),
            mean_rmse = mean(rmse, na.rm = TRUE))

ggplot(clim_obs, aes(x = date, y = sum_biovolume)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

write.csv(scores, './output/climatology_forecast_scores.csv',
          row.names = FALSE)
