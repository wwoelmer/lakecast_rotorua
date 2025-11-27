# plot biovolume at the four sites over time
library(tidyverse)
library(ggpubr)

########## read in biovolume data
boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv')%>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume)) %>% 
  group_by(Site, SampleDate) %>% 
  summarise(sum_biovolume = sum(PotentiallyToxicBioVolume)) %>% # sum the biovolume on a given site and day (following NERM Cyanobacteria protocol 2024, alert levels are based on the combined total of all cyanobacteria)
  mutate(doy = yday(SampleDate), # add week and day of year
         year = year(SampleDate),
         week = week(SampleDate))

# dataframe of warning levels for plotting
warnings <- data.frame(category = c('green', 'orange', 'red'),
                       ymin = c(0, 0.5, 10),
                       ymax = c(0.5, 10, Inf))


p_bv <- ggplot(boprc, aes(x = as.Date(SampleDate), y = as.numeric(sum_biovolume))) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Ngongotaha', 'Holdens Bay')), 
             scales = 'free') +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(boprc$fcast_date), xmax = max(boprc$fcast_date),
                fill = category,
                alpha = 0.2)) +
  geom_point() +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  ylab('Biovolume of Potentially Toxin Producing Species') +
  xlab('Date') +
  labs(fill = 'Warning Level') + 
  guides(alpha = 'none')
p_bv
ggsave('./figures/nzfss/biovolume_timeseries.png',
       p_bv, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')  

p_bv_cropped <- ggplot(boprc, aes(x = as.Date(SampleDate), y = as.numeric(sum_biovolume))) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Holdens Bay', 'Ngongotaha')), 
             scales = 'free') +
  ylim(0, 10) +
  geom_rect(data = warnings, inherit.aes = FALSE,
            aes(ymin = ymin, ymax = ymax, 
                xmin = min(boprc$fcast_date), xmax = max(boprc$fcast_date),
                fill = category,
                alpha = 0.2)) +
  geom_point() +
  scale_fill_manual(values = c('green', 'orange', 'red')) +
  ylab('Biovolume of Potentially Toxin Producing Species') +
  xlab('Date') +
  labs(fill = 'Warning Level') + 
  guides(alpha = 'none')
p_bv_cropped
ggsave('./figures/nzfss/biovolume_timeseries_cropped.png',
       p_bv_cropped, width = 250, height = 150, dpi = 300, scale = 0.7,
       unit = 'mm')  

p_bv_week_of_year <- ggplot(boprc, aes(x = week, y = as.numeric(sum_biovolume), color = as.factor(year))) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Holdens Bay', 'Ngongotaha')), 
             scales = 'free') +
  geom_point() +
  geom_line() +
  ylab('Biovolume of Potentially Toxin Producing Species') +
  xlab('Date') +
  theme_bw()
p_bv_week_of_year  
  

boprc %>% 
  group_by(Site, week) %>% 
  summarise(mean_bv = mean(sum_biovolume, na.rm = TRUE),
            sd = sd(sum_biovolume, na.rm = TRUE)) %>% 
  ggplot(aes(x = week, y = mean_bv)) +
  geom_ribbon(aes(ymin = mean_bv - sd, ymax = mean_bv + sd), alpha = 0.4) +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Ngongotaha', 'Holdens Bay')), 
             scales = 'free') +
  geom_point() +
  geom_line() +
  ylab('Biovolume of Potentially Toxin Producing Species') +
  xlab('Date') +
  theme_bw() 



a <- ggplot(boprc, aes(x = sum_biovolume)) +
  geom_histogram(fill = 'red') +
  theme_bw() +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Holdens Bay', 'Ngongotaha')), 
             scales = 'free') +
  xlab(expression(paste("Biovolume (mm"^3, " ", L^-1, ")")))

b <- ggplot(boprc, aes(x = log(sum_biovolume))) +
  geom_histogram(fill = 'red') +
  theme_bw() +
  facet_wrap(~factor(Site,
                     levels = c('Hamurana', 'Ohau Channel', 'Holdens Bay', 'Ngongotaha')), 
             scales = 'free') +
  xlab(expression(paste("Log Biovolume (mm"^3, " ", L^-1, ")")))

p_hist <- ggarrange(a, b)
p_hist
ggsave('./figures/nzfss/biovolume_histos.png',
       p_hist, width = 250, height = 100, dpi = 300, scale = 0.7,
       unit = 'mm')  


# summer 24-25 alone
summer_24_25 <- boprc %>% 
  filter(SampleDate > '2024-10-01') %>% 
  ggplot(aes(x = as.Date(SampleDate), y = as.numeric(sum_biovolume))) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  ylab(expression(paste("Potentially Toxic Biovolume (mm"^3, " ", L^-1, ")"))) +
  xlab('Date')

ggsave('./figures/nzfss/biovolume_targets_2024-2025.png',
       summer_24_25, width = 250, height = 100, dpi = 300, scale = 0.7,
       unit = 'mm')  


# mean and CV of obs at each site
summer_24_25_stats <- boprc %>% 
  filter(SampleDate > '2024-10-01') %>%
  group_by(Site) %>% 
  summarise(mean = mean(sum_biovolume, na.rm = TRUE),
            CV = sd(sum_biovolume, na.rm = TRUE)/mean)
summer_24_25_stats
