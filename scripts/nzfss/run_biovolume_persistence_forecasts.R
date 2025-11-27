
library(tidyverse)

source('./scripts/functions/biovolume_temporal_persistence.R')

##################################################################
# now run the forecasts
# read in the targets data

targets <-  tgt_ts

h_weeks = 4
start <- as.Date('2024-12-02')

## for 2025 forecasts 
forecast_dates <- targets$date[targets$date >= as.Date("2024-11-25")]
forecast_dates <- unique(forecast_dates)

forecast_sites <- expand.grid(start = forecast_dates) %>%
  mutate(h_weeks = 4)

forecast_sites

### run the forecasts
RW_2025 <- purrr::pmap_dfr(forecast_sites, forecast.RW)

# creat horizon column: calculate number of weeks
RW_2025 <- RW_2025 %>% 
  mutate(horizon = forecast_date - start_time) %>% 
  mutate(horizon_weeks = round(as.numeric(difftime(forecast_date, start_time, units = "weeks"))),
         horizon_label = paste0(horizon_weeks, " week", ifelse(horizon_weeks == 1, "", "s")))

# calculate mean and CI
RW_forecast_summary <- RW_2025 %>% 
  mutate(pred_og = expm1(predicted)) %>% 
  group_by(Site, start_time, horizon, horizon_label, forecast_date) %>% 
  summarise(median_pred = median(pred_og),
            lower_95 = pmax(0, quantile(pred_og, 0.025)),
            upper_95 = quantile(pred_og, 0.975),
            mean_pred = mean(pred_og),
            sd_pred = sd(pred_og))


ggplot(RW_forecast_summary, aes(x = forecast_date, y = median_pred)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = Site), alpha = 0.2) +
  geom_line(color = "blue") +
  geom_point(aes(x = forecast_date, y = median_pred, color = Site)) +
 # geom_point(data = last_obs, aes(x = as.Date(date), y = sum_biovolume)) +
  facet_wrap(Site~horizon_label, scales = 'free') +
  labs(x = "Date", y = "Biovolume") +
  theme_bw()


last_obs <- targets %>% 
  group_by(Site) %>% 
  filter(as.Date(date) == forecast_dates[1]) %>% 
  slice_tail(n = 3) %>% 
  select(Site, date, sum_biovolume) 

RW_forecast_summary %>% 
  filter(start_time==forecast_dates[1]) %>% 
  ggplot(aes(x = forecast_date, y = median_pred)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = Site), alpha = 0.2) +
  geom_line(color = "blue") +
  geom_point(aes(x = forecast_date, y = median_pred, color = Site)) +
  geom_vline(xintercept = forecast_dates[1]) +
  geom_point(data = last_obs, aes(x = as.Date(date), y = sum_biovolume)) +
  facet_wrap(~Site) +
  labs(x = "Date", y = "Biovolume") +
  theme_bw() +
  ggtitle(paste0('1-month ahead Temporal persistence forecast made on ', forecast_dates[2]))


# score the forecasts
# combine with obs

last_obs <- targets %>% 
  group_by(Site) %>% 
  filter(as.Date(date) %in% unique(as.Date(RW_forecast_summary$forecast_date))) %>% 
  select(Site, date, sum_biovolume) %>% 
  rename(forecast_date = date)

RW_2025_obs <- RW_2025 %>% 
  group_by(Site, start_time, horizon, horizon_label, forecast_date) %>% 
  summarise(mean_pred = mean(predicted), # in log-units, but modelled distribution is normal
            sd_pred = sd(predicted)) %>% 
  left_join(last_obs) %>% 
  filter(!is.na(sum_biovolume)) 
  
scores <- RW_2025_obs %>%
  group_by(Site, forecast_date, start_time, horizon) %>%
  summarise(crps = crps_lnorm(y = sum_biovolume, # in native units
                              mean =  mean_pred, # in log units
                              sd = sd_pred), # in log units
            rmse = sqrt(mean((expm1(mean_pred) - sum_biovolume)^2, na.rm = TRUE)) ) 


ggplot(scores, aes(x = forecast_date, y = crps, color = as.factor(horizon))) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  ggtitle('Temporal persistence scores')

ggplot(scores, aes(x = forecast_date, y = crps, color = as.factor(horizon))) +
  geom_point() +
  facet_wrap(horizon~Site) +
  theme_bw() +
  ggtitle('Temporal persistence scores')

summary_scores <- scores %>% 
  group_by(Site, horizon) %>% 
  summarise(mean_crps = mean(crps, na.rm = TRUE),
            mean_rmse = mean(rmse, na.rm = TRUE)) %>% 
  arrange(horizon)


ggplot(summary_scores, aes(x = horizon, y = mean_rmse)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

scores %>% 
  group_by(Site, horizon) %>% 
  summarise(mean_crps = mean(crps, na.rm = TRUE),
            mean_rmse = mean(rmse, na.rm = TRUE)) %>% 
  ggplot(aes(x = horizon, y = mean_crps, color = 'crps')) +
  geom_point() +
  facet_wrap(~Site)


ggplot(scores, aes(x = forecast_date, y = rmse, color = as.factor(horizon))) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

ggplot(RW_2025_obs, aes(x = forecast_date, y = sum_biovolume)) +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw()

write.csv(scores, './output/temporal_persistence_forecast_scores.csv',
          row.names = FALSE)

