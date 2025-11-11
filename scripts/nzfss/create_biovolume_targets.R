# temporal persistence at each site
library(zoo)
library(tidyverse)
library(tsibble)
source('./scripts/functions/yr_to_hydro_yr.R')

########## read in biovolume data
boprc <- read.csv('./data/boprc_cyano/boprc_cyano_2015-01-07_2025-05-26.csv')%>% 
  select(Location, Site, SampleDate, PotentiallyToxicBioVolume) %>% 
  filter(!is.na(PotentiallyToxicBioVolume)) %>% 
  group_by(Site, SampleDate) %>% 
  summarise(sum_biovolume = sum(PotentiallyToxicBioVolume)) %>%  # sum the biovolume on a given site and day (following NERM Cyanobacteria protocol 2024, alert levels are based on the combined total of all cyanobacteria)
  mutate(SampleDate = as.Date(SampleDate))

# create a week column and average by week and site for any dup samples
boprc <- boprc %>% 
  mutate(week = week(SampleDate),
         year = year(SampleDate)) %>% 
  group_by(Site, week, year) %>% 
  mutate(sum_biovolume = mean(sum_biovolume, na.rm = TRUE)) %>% 
  distinct(Site, week, year, .keep_all = TRUE)

ggplot(boprc, aes(x = SampleDate, y = sum_biovolume)) +
  geom_point() +facet_wrap(~Site)

# add NAs for weeks which are absent
boprc <- boprc %>% 
  group_by(Site) %>% 
  arrange(SampleDate, .by_group = TRUE) %>% 
  complete(SampleDate = seq(
    from = floor_date(min(SampleDate), unit = "week", week_start = 1), # Monday of first week
    to   = ceiling_date(max(SampleDate), unit = "week", week_start = 1) - 1, # last Sunday
    by   = "1 week"
  )) %>% 
  group_by(Site) %>% 
  arrange(SampleDate)

# recalculate week and year and average
boprc <- boprc %>% 
  group_by(Site) %>% 
  arrange(SampleDate, .by_group = TRUE) %>% 
  mutate(week = week(SampleDate),
         year = year(SampleDate)) %>% 
  mutate(week = ifelse(week==53, 52, week)) %>% # for years with 53 weeks, put it back to week 52 and average
  group_by(Site, week, year) %>% 
  mutate(sum_biovolume = mean(sum_biovolume, na.rm = TRUE)) %>% 
  distinct(Site, week, year, .keep_all = TRUE)

# interpolate if there are <2? 6? weeks between obs
boprc <- boprc %>% 
  group_by(Site) %>% 
  mutate(sum_biovolume = na.approx(sum_biovolume, x = SampleDate,
                                   maxgap = 6, na.rm = FALSE))

## create hydro year
boprc$date <- as.POSIXct(boprc$SampleDate)
boprc <- yr_to_hydro_yr(boprc) %>% 
  select(Site, date, hydroyear, sum_biovolume)

# create a 'week of hydroyear'
boprc <- boprc %>% 
  group_by(hydroyear, Site) %>% 
  mutate(hydroweek = row_number())

ggplot(boprc, aes(x = date, y = sum_biovolume, color = hydroweek)) +
  geom_point() +
  facet_wrap(~Site)

n_weeks_data <- boprc %>% 
  group_by(Site, hydroyear) %>% 
  summarise(n_weeks = n_distinct(hydroweek))

ggplot(n_weeks_data, aes(x = hydroyear, y = n_weeks, fill = Site)) +
  geom_col(position = 'dodge')

# creat lag column
boprc <- boprc %>% 
  group_by(Site, hydroyear) %>% 
  mutate(biovolume_lag = lag(sum_biovolume))

boprc_nogaps <- boprc %>% 
  arrange(Site, hydroyear, date) %>%
  group_by(Site, hydroyear) %>%
  # logical: TRUE when biovolume is observed
  mutate(non_na = !is.na(sum_biovolume),
         # create a run id using rle of the non_na vector
         run_id = {
           rr <- rle(non_na)
           rep(seq_along(rr$lengths), rr$lengths)
         }) %>%
  # keep only runs that are non-NA (we only want runs of observed values)
  filter(non_na) %>%
  # summarise each run
  group_by(Site, hydroyear, run_id) %>%
  summarise(run_start = min(date),
            run_end   = max(date),
            run_length = n(),   # number of consecutive non-NA observations
            .groups = "drop") %>% 
  group_by(Site, hydroyear) %>% 
  filter(run_length==max(run_length))

ggplot(boprc_nogaps, aes(x = hydroyear, y = run_length, fill = Site)) +
  geom_col(position = 'dodge')

# no use the nogaps dataframe to subset boprc
boprc_clean <- boprc %>% 
  left_join(boprc_nogaps) %>%
  group_by(Site, hydroyear) %>%
  filter(date >= run_start & date <= run_end) %>%
  select(-run_start, -run_end) 

# clean up column order
boprc_clean <- boprc_clean %>% 
  select(Site, date, hydroyear, hydroweek, sum_biovolume, biovolume_lag, run_length)

ggplot(boprc_clean, aes(x = as.Date(date), y = sum_biovolume, color = run_length)) +
  geom_point() +
  facet_wrap(~Site) +
  scale_color_viridis_b() +
  theme_bw() +
  xlab('Date') +
  ylab('Biovolume')  +
  labs(color = '# consecutive weeks')

# clean up df
tgt_ts <- boprc_clean %>% 
  mutate(log_biovolume = log1p(sum_biovolume)) %>% 
  select(Site, date, hydroyear, hydroweek, sum_biovolume, log_biovolume) %>% 
  mutate(observed = log_biovolume)#

write.csv(tgt_ts, './data/biovolume_targets.csv', row.names = FALSE)
