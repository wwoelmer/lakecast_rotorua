library(tidyverse)
library(ggpubr)
library(scoringRules)

# read in targets
targets <- read.csv('./data/biovolume_targets.csv')
targets$date <- as.Date(targets$date)

################################################################################
# read in profiling buoy data
buoy <- read.csv('./data/buoy/rotorua_profiles_latest.csv')
buoy_daily <- buoy %>% 
  pivot_longer(TmpWtr:FlPhyc, names_to = 'variable', values_to = 'value') %>% 
  mutate(date = as.Date(DateTime)) %>% 
  group_by(date, variable, DptSns) %>% 
  summarise(value = mean(value, na.rm = TRUE))

buoy_daily <- buoy_daily %>% 
  filter(variable %in% c('FlChlr', 'FlPhyc'))

# calculate depth-integrated chl and phyco on each day across water col
# For var_integrated, what you're doing is adding up each pair of neighboring depths, 
# then dividing by two to get the average value at that depth interval, 
# then you multiply by the distance (m) between each depth to standardize, 
# and add it all up for the integration

buoy_int <- buoy_daily %>% 
  filter(!is.na(value) & !is.na(DptSns)) %>%
  arrange(DptSns) %>%
  group_by(date, variable) %>% 
  summarise(var_integrated = sum(diff(DptSns, na.rm = TRUE) * (head(value, -1) + tail(value, -1)) / 2),
            var_int_avg = var_integrated/max(DptSns)) 

# Var_integrated is the sum of the depth integrated variable, so for chl fluorescence for example, it would be in units of RFU-m
# Var_int_avg  is the depth integrated variable, but standardized by the maximum depth, so it returns to units of RFU

ggplot(buoy_int, aes(x = as.Date(date), y = var_int_avg)) +
  geom_point() +
  facet_wrap(~variable, scale = 'free')

# make wide format
buoy_wide <- buoy_int %>% 
  select(date, variable, var_int_avg) %>% 
  pivot_wider(names_from = 'variable', values_from = 'var_int_avg') %>% 
  rename('phyco_RFU_int_avg' = 'FlPhyc',
         'chl_RFU_int_avg' = 'FlChlr')

# create lags up to one week
buoy_wide <- buoy_wide %>% 
  arrange(date) %>% 
  ungroup() %>% 
  mutate(lag1_phyco = lag(phyco_RFU_int_avg, n = 1L),
         lag2_phyco = lag(phyco_RFU_int_avg, 2),
         lag3_phyco = lag(phyco_RFU_int_avg, 3),
         lag4_phyco = lag(phyco_RFU_int_avg, 4),
         lag5_phyco = lag(phyco_RFU_int_avg, 5),
         lag6_phyco = lag(phyco_RFU_int_avg, 6),
         lag7_phyco = lag(phyco_RFU_int_avg, 7)) %>% 
  mutate(lag1_chl = lag(chl_RFU_int_avg, 1),
         lag2_chl = lag(chl_RFU_int_avg, 2),
         lag3_chl = lag(chl_RFU_int_avg, 3),
         lag4_chl = lag(chl_RFU_int_avg, 4),
         lag5_chl = lag(chl_RFU_int_avg, 5),
         lag6_chl = lag(chl_RFU_int_avg, 6),
         lag7_chl = lag(chl_RFU_int_avg, 7))

a <- buoy_wide %>% 
  filter(date > '2024-11-30') %>% 
  ggplot(aes(x = as.Date(date), y = phyco_RFU_int_avg, color = 'buoy phyco RFU')) +
  xlim(as.Date('2024-11-30'), as.Date('2025-05-31')) + 
  ylim(0, 20) +
  geom_point() +
  geom_line() +
  geom_point(data = targets[targets$date > as.Date('2021-11-30'),], aes(x = as.Date(date), y = sum_biovolume, color = Site)) +
  geom_line(data = targets[targets$date > as.Date('2021-11-30'),], aes(x = as.Date(date), y = sum_biovolume, color = Site)) +
  theme_bw()
  
a
################################################################################
# combine the dataframes
buoy_bv <- left_join(targets, buoy_wide)

# remove days from before profiling buoy went out
buoy_bv <- buoy_bv %>% 
  filter(date > min(buoy_int$date))

p1 <- ggplot(buoy_bv, aes(x = log(sum_biovolume), y = log(phyco_RFU_int_avg))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  ylab('Log of Depth Averaged Fluorescence (RFU)') +
  xlab('Log of Toxic Biovolume (mm3/L)') +
  theme_bw() 

ggsave('./figures/nzfss/bv_phyco_relationship.png',
       p1, width = 250, height = 200, dpi = 300, scale = 0.6,
       unit = 'mm')  

# clean up some columns 
buoy_bv <- buoy_bv %>% 
  select(Site, date, sum_biovolume, chl_RFU_int_avg, phyco_RFU_int_avg, lag1_phyco:lag7_chl)


#### function stuff
source('scripts/functions/fit_buoy_bv.R')

dates <- seq.Date(as.Date('2024-11-25'), as.Date('2025-05-21'), by = 'week')
dates <- unique(buoy_bv$date)
dates <- dates[dates >= as.Date('2024-11-25')]

sites <- unique(buoy_bv$Site)
out <- data.frame()
for(i in 1:length(dates)){
  for(j in 1:length(sites)){
    print(dates[i])
    print(sites[j])
    temp <- fit_buoy_bv(data = buoy_bv,
                        fcast_start = dates[i],
                        horizon = 1,
                        lag = 1,
                        fcast_site = sites[j])
    out <- rbind(out, temp)
  }
}

p <- ggplot(out, aes(x = date, y = pred_bv_native, color = 'Buoy predicted')) +
  geom_point() +
  geom_line() +
  #geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "skyblue", alpha = 0.3) +
  geom_point(aes(x = date, y = observed_bv, color = 'Obs Biovolume')) +
  geom_line(aes(x = date, y = observed_bv, color = 'Obs Biovolume')) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  scale_color_manual(values = c('#0072B2', 'red')) +
  theme_bw() +
  labs(color = NULL) +
  ylab(expression(paste("Potentially Toxic Biovolume (mm"^3, " ", L^-1, ")"))) 
p  
ggsave('./figures/nzfss/buoy_obs_bv_comparison.png', p,
       width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')

# look at just during the bloomy times
out %>% 
  filter(date < '2025-01-15') %>% 
  ggplot(aes(x = date, y = pred_bv_native, color = 'pred')) +
  geom_point() +
  geom_line() +
  #geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), fill = "skyblue", alpha = 0.3) +
  geom_point(aes(x = date, y = observed_bv, color = 'obs')) +
  geom_line(aes(x = date, y = observed_bv, color = 'obs')) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel',
                                'Ngongotaha', 'Holdens Bay'))) +
  theme_bw()

# make a gif 
for(i in 1:length(dates)){
  # look at a few days at a time around peak bloom dynamics
  out_sub <- out %>% 
    filter(as.Date(date)<=dates[i]) %>% 
    group_by(date, Site) %>% 
    mutate(diff_btw_buoy_shore = observed_bv - pred_bv_native)
  
  p <- ggplot(out_sub) +
      geom_point(aes(x = date, y = diff_btw_buoy_shore)) +
      geom_line(aes(x = date, y = diff_btw_buoy_shore)) +
      facet_wrap(~factor(Site,
                         levels = c('Hamurana', 'Ohau Channel',
                                    'Ngongotaha', 'Holdens Bay'))) +
      theme_bw() 
  p
  ggsave(paste0("./figures/nzfss/biovolume_frames/biovolume_",dates[i], ".png"), p,
         width = 250, height = 150, dpi = 300, scale = 0.7,
         unit = 'mm')
  

}

# Combine into an animated GIF
frames <- list.files("./figures/nzfss/biovolume_frames", full.names = TRUE, pattern = "*.png")
frames <- frames[order(frames)]  # Ensure correct order
img_list <- image_read(frames)
img_gif <- image_animate(img_list, fps = 1)  # adjust fps for speed
image_write(img_gif, "./figures/nzfss/biovolume_animation.gif")
image_write_video(img_list, path = "./figures/nzfss/biovolume_animation.mp4",
                  framerate = 1)


out <- out %>% 
  group_by(Site, date) %>% 
  mutate(rmse = sqrt(mean((pred_bv_native - observed_bv)^2, na.rm = TRUE)),
         sd_est = (pred_upper - pred_lower)/(2*1.96),
         crps = crps_norm(y = observed_bv, mean = pred_bv_native, sd = sd_est))

ggplot(out, aes(x = date, y = rmse)) +
  geom_point() +
  facet_wrap(~Site)

ggplot(out, aes(x = date, y = crps)) +
  geom_point() +
  facet_wrap(~Site)

write.csv(out, './output/buoy_persistence_forecast_scores.csv', row.names = FALSE)
