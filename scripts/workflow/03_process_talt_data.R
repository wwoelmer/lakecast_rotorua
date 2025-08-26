#install.packages("Microsoft365R")
library(Microsoft365R)
library(readxl)
library(ggpubr)
library(tidyverse)

token <- readRDS("token_onedrive.rds")
onedrive <- get_business_onedrive(token = token)

onedrive$download_file("Documents/U of Waikato/MBIE Smart Ideas Forecasting 2024/QuickFluor/LakeCast_data_digitized.xlsx", 
                       dest = "./data/talt_cyano/talt_cyano_digitized.xlsx",
                       overwrite = TRUE)

talt <- read_excel('./data/talt_cyano/talt_cyano_digitized.xlsx') 

talt$Date <- as.Date(dmy(talt$Date))

# fill in dates, times, and sites
talt_filled <- talt %>% 
  mutate(across(!Date, ~ ifelse(. == "", NA, .))) %>% 
  fill(Date, .direction = 'down') %>% 
  group_by(Date) %>% 
  fill(Site, .direction = 'down') %>% 
  group_by(Date, Site) %>% 
  fill(Time, .direction = 'down') 

# fill in fluoroquik ID by date
talt_filled <- talt_filled %>%
  ungroup() %>% 
  group_by(Date) %>% 
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
  group_by(Date, Site) %>% 
  filter(!if_all(c(FQ_chl, CF_chl, chl_RFU, maramataka), is.na))
  
# format some columns
talt_filled$Date <- as.Date(talt_filled$Date, format = "%d/%m/%Y")
talt_filled$FQ_ratio <- as.numeric(talt_filled$FQ_ratio)
talt_filled$DO_pct <- as.numeric(talt_filled$DO_pct)
talt_filled$FQ_chl <- as.numeric(talt_filled$FQ_chl)
talt_filled$CF_chl <- as.numeric(talt_filled$CF_chl)

# remove any extra spaces on site column
talt_filled$Site <- trimws(talt_filled$Site)

# force FQ phycocyanin to numeric
talt_filled$FQ_PH <- as.numeric(talt_filled$FQ_PH)

a <- ggplot(talt_filled, aes(x = Date, y = as.numeric(FQ_chl), color = FQ_ID, group = Date)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site, scales = 'free') +
  ylab('FluoroQuick Chl-a') +
  labs(color = 'Instrument ID') +
  theme_bw() +
  ggtitle('FluoroQuik Chl-a')

b <- ggplot(talt_filled, aes(x = Date, y = as.numeric(FQ_PH), color = FQ_ID, group = Date)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Site, scales = 'free') +
  ylab('FluoroQuick Phycocyanin') +
  labs(color = 'Instrument ID') +
  theme_bw() +
  ggtitle('FluoroQuik Phycocyanin')

p1 <- ggarrange(a, b, common.legend = TRUE,
          ncol = 1)

ggsave(paste0('./figures/monitoring_data_', Sys.Date(), '.png'),
       dpi = 300, width = 7, height = 10)

write.csv(talt_filled, 
          paste0('./data/talt_cyano/talt_cyano_formatted_', 
                 min(talt_filled$Date), '_',
                 max(talt_filled$Date), '.csv'),
          row.names = FALSE)

##### read in climatology