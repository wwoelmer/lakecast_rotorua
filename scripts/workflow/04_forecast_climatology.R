# create climatology forecast for the next week

fcast_date <- as.Date('2025-01-02') #Sys.Date()
fcast_week <- week(fcast_date)
max_horizon <- 14*20
fcast_site <- 'Hamurana'

warnings <- data.frame(category = c('green', 'orange', 'red'),
                       ymin = c(0, 0.5, 10),
                       ymax = c(0.5, 10, Inf))

fcast_df <- data.frame(fcast_start_date = fcast_date,
                       fcast_date = seq.Date(fcast_date, fcast_date + max_horizon - 1, by = 'day'),
                       horizon = 1:max_horizon)

fcast_df <- fcast_df %>% 
  mutate(week = week(fcast_date))


clim <- read.csv('./forecasts/climatology_values_probabilities.csv')
clim <- clim %>% 
  filter(week  %in% fcast_df$week)


fcast_df2 <- left_join(fcast_df, clim)
fcast_df2 <- na.omit(fcast_df2)

p1 <- ggplot(fcast_df2, aes(x = fcast_date, y = mean_raw)) +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(fcast_df$fcast_date), xmax = max(fcast_df$fcast_date),
                fill = category,
                alpha = 0.2)) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  geom_point() +
  geom_ribbon(aes(ymin = mean_raw - sd_raw, ymax = mean_raw + sd_raw),
              alpha = 0.3) +
  xlab('Date') +
  ylab('Climatology Forecasted Biovolume (UNITS)') +
  theme_bw() +
  coord_cartesian(ylim = c(0, NA)) +  # lower limit = 0, upper limit = auto
  labs(fill = 'Warning Level') +
  facet_wrap(~Site, scales = 'free') +
  guides(alpha = 'none') +
  ggtitle('Climatology forecast at BOPRC sites')
p1

ggplot(fcast_df2, aes(x = fcast_date, y = mean_raw, color = Site)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_raw - sd_raw, 
                  ymax = mean_raw + sd_raw,
                  fill = Site),
              alpha = 0.3) +
  xlab('Date') +
  ylab('Climatology Forecasted Biovolume (UNITS)') +
  theme_bw() +
  coord_cartesian(ylim = c(0, NA)) +  # lower limit = 0, upper limit = auto
  labs(fill = 'Warning Level') +
  guides(alpha = 'none') +
  ggtitle('Climatology forecast for the next two weeks')

ggsave(paste0('./figures/climatology_forecasts/climatology_', fcast_date, '.png'),
       dpi = 300, width = 8, height = 7)



    
    
    