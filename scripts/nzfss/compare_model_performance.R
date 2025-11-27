# compare RMSE across forecasts
library(tidyverse)

tp <- read.csv('./output/temporal_persistence_forecast_scores.csv') %>% 
  mutate(model = 'temporal persistence') %>% 
  filter(horizon==1) %>% 
  rename(forecast_date = date)

clim <- read.csv('./output/climatology_forecast_scores.csv') %>% 
  mutate(model = 'climatology') %>% 
  rename(forecast_date = date)

buoy <- read.csv('./output/buoy_persistence_forecast_scores.csv') %>% 
  mutate(model = 'buoy persistence') %>% 
  rename(forecast_date = date) 

scores <- full_join(tp, clim)
scores <- full_join(scores, buoy)


p <- ggplot(scores, aes(x = as.Date(forecast_date), y = rmse, color = model)) +
  geom_point() +
  geom_line() +
  facet_wrap(~factor(Site, 
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  theme_bw() +
  scale_color_manual(values = c('#0072B2',
                                '#E69F00',
                                '#F8766D')) +
  ylab(expression(paste("RMSE (mm"^3, " ", L^-1, ")"))) +
  xlab('Date') 


ggsave('./figures/nzfss/compare_all_models_RMSE.png', p,
       width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')

ggplot(scores, aes(x = as.Date(forecast_date), y = rmse, color = model)) +
  geom_point() +
  geom_line() +
  facet_wrap(~factor(Site, 
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  theme_bw() +
  scale_color_manual(values = c('#0072B2',
                                '#E69F00',
                                '#F8766D')) +
  ylab(expression(paste("RMSE (mm"^3, " ", L^-1, ")"))) +
  xlab('Date') +
  xlim(as.Date(min(scores$forecast_date), as.Date('2025-01-15')))

summary <- scores %>% 
  group_by(Site, model) %>% 
  summarise(mean_rmse = mean(rmse, na.rm = TRUE))
summary

summary_all <- scores %>% 
  group_by(model) %>% 
  summarise(mean_rmse = mean(rmse, na.rm = TRUE))
summary_all

######
ggplot(buoy, aes(x = as.Date(forecast_date), y = pred_bv_native, color = 'buoy forecasted')) +
  geom_point() +
  geom_line() +
  geom_point(aes(x = as.Date(forecast_date), y = observed_bv, color = 'observed')) +
  geom_line(aes(x = as.Date(forecast_date), y = observed_bv, color = 'observed')) +
  facet_wrap(~Site) +
  scale_color_manual(values = c('#4682B4', 'black')) +
  theme_bw() +
  ylab(expression(paste("Potentially Toxic Biovolume (mm"^3, " ", L^-1, ")"))) +
  xlab('Date') +
  ggtitle('Forecast 1-day before Biovolume measurements')
