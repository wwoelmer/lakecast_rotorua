# create climatology forecast for the next week
library(tidyverse)
library(Metrics)
library(scoringRules)

fcast_date <- as.Date('2025-01-02') #Sys.Date()
fcast_week <- week(fcast_date)
fcast_site <- 'Hamurana'

warnings <- data.frame(category = c('green', 'orange', 'red'),
                       ymin = c(0, 0.5, 10),
                       ymax = c(0.5, 10, Inf))

fcast_df <- data.frame(fcast_date = seq.Date(fcast_date, fcast_date + max_horizon - 1, by = 'day'))

fcast_df <- fcast_df %>% 
  mutate(week = week(fcast_date))

clim <- read.csv('./forecasts/climatology_values_probabilities.csv')
clim <- clim %>% 
  filter(week  %in% fcast_df$week)

fcast_df <- left_join(fcast_df, clim)
fcast_df <- na.omit(fcast_df)

p1 <- ggplot(fcast_df, aes(x = fcast_date, y = mean_pred)) +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(fcast_df$fcast_date), xmax = max(fcast_df$fcast_date),
                fill = category,
                alpha = 0.2)) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  geom_point() +
  geom_ribbon(aes(ymin = mean_pred - sd_pred, ymax = mean_pred + sd_pred),
              alpha = 0.3) +
  xlab('Date') +
  ylab('Climatology Forecasted Biovolume (UNITS)') +
  theme_bw() +
  coord_cartesian(ylim = c(0, NA)) +  # lower limit = 0, upper limit = auto
  labs(fill = 'Warning Level') +
  facet_wrap(~Site) +
  guides(alpha = 'none') +
  ggtitle('Climatology forecast at BOPRC sites')
p1

p2 <- ggplot(fcast_df, aes(x = fcast_date, y = prob, fill = warnings)) +
  geom_col(position = 'stack') +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  xlab('Date') +
  ylab('Climatology Forecasted Likeilhood of Warnings') +
  theme_bw() +
  coord_cartesian(ylim = c(0, NA)) +  # lower limit = 0, upper limit = auto
  labs(fill = 'Warning Level') +
  facet_wrap(~Site) +
  guides(alpha = 'none') +
  ggtitle('Climatology forecast at BOPRC sites')
p2

# bring on observations to assess with data
fcast_tgt <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv') %>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume)) %>% 
  group_by(Site, SampleDate) %>% 
  summarise(sum_biovolume = sum(PotentiallyToxicBioVolume)) %>% 
  mutate(doy = yday(SampleDate), # add week and day of year
         year = year(SampleDate),
         week = week(SampleDate)) %>% 
  filter(year >= fcast_year)


# one dataframe for continuous 
fcast_con <- fcast_df %>% 
  select(fcast_date:sd_pred) %>% 
  left_join(fcast_tgt, by = c('week', 'Site')) %>% 
  group_by(week, Site) %>% 
  mutate(rmse = rmse(sum_biovolume, mean_pred))

rmse <- fcast_con %>% 
  group_by(Site) %>% 
  summarise(mean_rmse = mean(rmse, na.rm = TRUE))
rmse

ggplot(fcast_con, aes(x = week, y = rmse)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

# if we subset to after the first week
fcast_con %>% 
  filter(week > 2) %>% 
ggplot(aes(x = week, y = rmse)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

library(ggbreak)
library(ggforce)
ggplot(fcast_con, aes(x = week, y = rmse, color = Site)) +
  geom_point() +
  facet_wrap(~Site) +
  #facet_zoom(y < 0.5) +
  theme_bw()+
  scale_y_break(c(0.5, 2)) 
  

# and one for categorical
fcast_tgt_cat <- fcast_tgt %>% 
  mutate(obs_warning_level = case_when(sum_biovolume < 0.5 ~ "Green",
                                       sum_biovolume >= 0.5 & sum_biovolume < 10 ~ 'Orange',
                                       sum_biovolume >= 10 ~ 'Red'))
fcast_cat <- fcast_df %>% 
  select(week, Site, warnings, prob) %>%
  distinct(week, Site, warnings, .keep_all = TRUE) %>% 
  left_join(fcast_tgt_cat, by = c('week', 'Site')) %>% 
  pivot_wider(names_from = 'warnings', values_from = 'prob') %>% 
  mutate(Orange = ifelse(is.na(Orange), 0, Orange),
         Red = ifelse(is.na(Red), 0, Red))

fcast_cat <- na.omit(fcast_cat)

brier <- mean(
  (fcast_cat$Green - (fcast_cat$obs_warning_level=="Green"))^2 +
    (fcast_cat$Orange - (fcast_cat$obs_warning_level=="Orange"))^2 +
    (fcast_cat$Red - (fcast_cat$obs_warning_level=="Red"))^2
)
brier

fcast_cat_long <- fcast_df %>% 
  select(week, Site, warnings, prob) %>%
  distinct(week, Site, warnings, .keep_all = TRUE) %>% 
  left_join(fcast_tgt_cat, by = c('week', 'Site')) 

fcast_cat_long <- na.omit(fcast_cat_long)
  
ggplot(fcast_cat_long, aes(x = week, y = prob, fill = warnings)) +
  geom_area(position = "stack", alpha = 0.6) +
  geom_point(aes(x = week, y = 1.05, color = obs_warning_level), size = 2) +
  scale_y_continuous(limits = c(0, 1.2)) +
  facet_wrap(~Site) +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  scale_color_manual(values = c('green', 'orange', 'red')) +
  labs(x = "Week", y = "Predicted probability", fill = "Predicted class", color = "Observed") +
  theme_minimal()


ggsave(paste0('./figures/climatology_forecasts/climatology_', fcast_date, '.png'),
       dpi = 300, width = 8, height = 7)



    
    
    