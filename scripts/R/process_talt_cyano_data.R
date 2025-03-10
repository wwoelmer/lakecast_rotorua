library(ggpubr)


talt <- read.csv('./data/talt_cyano/LakeCast_data_digitized(Sheet1).csv') %>% 
  select(-X)

# fill in dates, times, and sites
talt_filled <- talt %>% 
  mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>% 
  fill(Date, .direction = 'down') %>% 
  group_by(Date) %>% 
  fill(Site, .direction = 'down') %>% 
  group_by(Date, Site) %>% 
  fill(Time, .direction = 'down') %>% 
  fill(FQ_ID, .direction = 'down')

# fill in ProDSS measurements, weaterh & water description based on date-site
talt_filled <- talt_filled %>% 
  group_by(Date, Site) %>% 
  fill(temperature_C:chl_RFU, .direction = 'down') %>% 
  fill(weather_description:water_description, .direction = 'down')

# fill in maramataka based on date only
talt_filled <- talt_filled %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  fill(maramataka, .direction = 'down')

# there are some rows with no data at all, look across several columns to filter these rows
talt_filled <- talt_filled %>% 
  group_by(Date) %>% 
  filter(!is.na(FQ_PH) & !is.na(FQ_chl) & !is.na(CF_PH) & !is.na(CF_chl))

# format some columns
talt_filled$Date <- as.Date(talt_filled$Date, format = "%d/%m/%Y")
talt_filled$FQ_ratio <- as.numeric(talt_filled$FQ_ratio)
talt_filled$DO_pct <- as.numeric(talt_filled$DO_pct)

ggplot(talt_filled, aes(x = Date, y = FQ_chl, color = Site)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site) +
  ylab('FluoroQuick Chl-a') +
  theme_bw()

ggplot(talt_filled, aes(x = Date, y = CF_chl, color = Site)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site)

ggplot(talt_filled, aes(x = Date, y = FQ_PH, color = Site)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site)

ggplot(talt_filled, aes(x = Date, y = CF_PH, color = Site)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site)


a <- ggplot(talt_filled, aes(x = FQ_PH, y = CF_PH, color = Site)) +
  geom_point(size = 2) +
  theme_bw() +
  xlab('FluoroQuick Phycocyanin') +
  ylab('CyanoFluor Phycocyanin')

b <- ggplot(talt_filled, aes(x = FQ_chl, y = CF_chl, color = Site)) +
  geom_point(size = 2) +
  theme_bw() +
  xlab('FluoroQuick chl-a') +
  ylab('CyanoFluor chl-a')

ggplot(talt_filled, aes(x = FQ_chl, y = chl_RFU, color = Site)) +
  geom_point(size = 2) +
  theme_bw() +
  xlab('FluoroQuick chl-a') +
  ylab('ProDSS chl-a')

ggarrange(a, b, common.legend = TRUE)

ggplot(talt_filled, aes(x = FQ_ratio, y = CF_ratio, color = Site)) +
  geom_point() +
  facet_wrap(~Site, scales = 'free') +
  geom_smooth(method = 'lm')

talt_long <- talt_filled %>% 
  pivot_longer('FQ_PH':'chl_RFU', names_to = 'variable', values_to = 'value')

talt_long$variable <- factor(talt_long$variable,
                             levels = c('FQ_chl', 'CF_chl', 'chl_RFU',
                                        'FQ_PH', 'CF_PH', 'FQ_ratio', 'CF_ratio',
                                        'temperature_C', 'DO_pct', 'turbidity'))

ggplot(talt_long, aes(x = as.Date(Date), y = value, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = 'free', ncol = 3) +
  theme_bw() +
  xlab('Date') 
