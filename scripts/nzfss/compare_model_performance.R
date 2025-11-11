# compare RMSE across forecasts
library(tidyverse)

tp <- read.csv('./output/temporal_persistence_forecast_scores.csv') %>% 
  mutate(model = 'temporal persistence') %>% 
  filter(horizon==7)

clim <- read.csv('./output/climatology_forecast_scores.csv') %>% 
  mutate(model = 'climatology') %>% 
  rename(forecast_date = date)

buoy <- read.csv('./output/buoy_persistence_forecast_scores.csv') %>% 
  mutate(model = 'buoy persistence') %>% 
  rename(forecast_date = date) 

scores <- full_join(tp, clim)
scores <- full_join(scores, buoy)


ggplot(scores, aes(x = as.Date(forecast_date), y = rmse, color = model)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

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
