# score climatology forecasts
library(tidyverse)
library(scoringRules)

# read in climatology forecasts
clim <- read.csv('./forecasts/climatology_values_probabilities.csv')

# read in targets
targets <- read.csv('./data/biovolume_targets.csv')


# pull 2025 forecast dates
targets <- targets %>% 
  filter(date > as.Date("2024-11-25"))

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

scores$Site <- factor(scores$Site, levels = c('Hamurana', 'Ohau Channel', 'Ngongotaha', 'Holdens Bay'))

ggplot(scores, aes(x = as.Date(date), y = crps)) +
  facet_wrap(~Site) +
  theme_bw() +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(boprc$fcast_date), xmax = max(boprc$fcast_date),
                fill = category,
                alpha = 0.2)) +
  ggtitle('Climatology scores') +
  geom_point() +
  geom_line() +
  guides(alpha = 'none') +
  scale_fill_manual(values = c('green', 'orange', 'red'),
                    name = 'Warning Level') +
  xlab('Date') +
  ylab(expression(paste("CRPS (mm"^3, " ", L^-1, ")")))


# dataframe of warning levels for plotting
warnings <- data.frame(category = c('green', 'orange', 'red'),
                       ymin = c(0, 0.5, 10),
                       ymax = c(0.5, 10, Inf))


clim_scores <- ggplot(scores, aes(x = as.Date(date), y = rmse)) +
  facet_wrap(~factor(Site, 
                     levels = c('Hamurana', 'Ohau Channel', 'Ngongotaha', 'Holdens Bay'))) +
  theme_bw() +
  ggtitle('Climatology scores') +
  geom_point() +
  geom_line() +
  guides(alpha = 'none') +
  xlab('Date') +
  ylab(expression(paste("RMSE (mm"^3, " ", L^-1, ")")))

ggsave('./figures/nzfss/RMSE_climatology_24-25.png',
       clim_scores, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')    

scores %>% 
  group_by(Site) %>% 
  summarise(mean_crps = mean(crps, na.rm = TRUE),
            mean_rmse = mean(rmse, na.rm = TRUE))



obs_bv <- ggplot(clim_obs, aes(x = as.Date(date), y = sum_biovolume)) +
  facet_wrap(~factor(Site, 
                     levels = c('Hamurana', 'Ohau Channel', 'Ngongotaha', 'Holdens Bay'))) +
  theme_bw() +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(boprc$fcast_date), xmax = max(boprc$fcast_date),
                fill = category,
                alpha = 0.2)) +
  ggtitle('Observed biovolume summer 2024-2025') +
  geom_point() +
  geom_line() +
  guides(alpha = 'none') +
  scale_fill_manual(values = c('green', 'orange', 'red'),
                    name = 'Warning Level') +
  xlab('Date') +
  ylab(expression(paste("Biovolume (mm"^3, " ", L^-1, ")")))

ggsave('./figures/nzfss/biovolume_targets_24-25.png',
       obs_bv, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')   
  
  write.csv(scores, './output/climatology_forecast_scores.csv',
          row.names = FALSE)
