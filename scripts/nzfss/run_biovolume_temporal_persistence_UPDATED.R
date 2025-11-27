# try new rw function
library(tidyverse)
library(Metrics)
source('./scripts/functions/biovolume_random_walk.R')

# dataframe of warning levels for plotting
warnings <- data.frame(category = c('green', 'orange', 'red'),
                       ymin = c(0, 0.5, 10),
                       ymax = c(0.5, 10, Inf))

targets <- read.csv( './data/biovolume_targets.csv')

# first date of the 2024-2025 season
start_date <- as.Date('2024-11-25')

# create date vector
dates <- unique(targets$date[targets$date >= start_date])


out <- NULL
for(i in 1:length(dates)){
  # filter to after start date in loop
  tmp <- targets %>% 
    filter(date <= dates[i])
  
  rw_fcast <- tmp %>% 
    group_by(Site) %>% 
    summarise(fcast = list(rw_forecast_log(sum_biovolume, horizon = 4)), .groups = 'drop') %>% 
    unnest(fcast)
  
  rw_fcast <- rw_fcast %>% 
    mutate(start_date = dates[i])
  
  # get horizon dates
  fcast_dates <- data.frame(date = dates[i:(i+4)])
  fcast_dates$horizon <- 0:4
  
  rw_fcast <- left_join(rw_fcast, fcast_dates)
  
  p <- ggplot(rw_fcast, aes(x = as.Date(date), y = mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
    geom_line(color = "blue") +
    geom_point(aes(x = head(as.Date(date), 1), y = last_obs)) +
    theme_minimal() +
    facet_wrap(~factor(Site,
                       levels = c('Hamurana', 'Ohau Channel',
                                  'Ngongotaha', 'Holdens Bay'))) +
    labs(title = paste0("Random Walk Forecast made on ", dates[i]), 
         x = "Forecast Date", 
         y = expression(paste("Forecasted Biovolume (mm"^3, " ", L^-1, ")")))
  
  ggsave(paste0('./figures/nzfss/temporal_persistence/temporal_persistence_forecast_', dates[i], '.png'),
         p, width = 250, height = 150, dpi = 300, scale = 0.7,
         unit = 'mm')  
  
  out <- rbind(out, rw_fcast)
}


# add back with observations from targets
obs <- targets %>% 
  select(Site, date, observed) %>% 
  arrange(Site, date)

out2 <- out %>% 
  left_join(obs)

out2 <- na.omit(out2)

# score them
out2 <- out2 %>% 
  group_by(Site, date, horizon) %>% 
  mutate(rmse = rmse(observed, mean))

ggplot(out2, aes(x = as.Date(date), y = rmse, color = as.factor(horizon))) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site)

p1 <- out2 %>% 
  filter(horizon==1) %>% 
  ggplot(aes(x = as.Date(date), y = rmse)) +
#geom_rect(data = warnings, inherit.aes = FALSE,
#          aes(ymin = ymin, ymax = ymax, 
#              xmin = min(boprc$fcast_date), xmax = max(boprc$fcast_date),
#              fill = category,
#              alpha = 0.2)) +
#scale_fill_manual(values = c('green', 'orange', 'red')) +
facet_wrap(~factor(Site, 
                   levels = c('Hamurana', 'Ohau Channel',
                              'Ngongotaha', 'Holdens Bay'))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab(expression(paste("RMSE (mm"^3, " ", L^-1, ")"))) +
  xlab('Date') +
  ylim(0, 10) +
  labs(fill = 'Warning Level') + 
  guides(alpha = 'none') +
  ggtitle('Temporal persistence scores')
p1
ggsave('./figures/nzfss/tp_RMSE.png',
       p1, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')   

out2 %>% 
  group_by(Site, horizon) %>% 
  summarise(rmse = mean(rmse))


p2 <- out2 %>% 
  group_by(Site, horizon) %>% 
  summarise(rmse = mean(rmse)) %>% 
  ggplot(aes(x = horizon, y = rmse)) +
  geom_point() +
  geom_line() +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  ylab(expression(paste("RMSE (mm"^3, " ", L^-1, ")"))) +
  xlab('Horizon (weeks ahead)') +
  theme_bw()

ggsave('./figures/nzfss/RMSE_over_horizon_tp.png',
       p2, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')  

write.csv(out2, './output/temporal_persistence_forecast_scores.csv', row.names = FALSE)
