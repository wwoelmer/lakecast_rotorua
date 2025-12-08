library(ggpubr)
library(tidyverse)
library(googlesheets4)

url <- 'https://docs.google.com/spreadsheets/d/1GPNLRdMJmfWd4c8rS2WneGTQpzIWs9IzB7YbruZSQFw/edit?usp=sharing'

talt <- read_sheet(url, col_types = 'c')

#talt <- read.csv('./data/talt_cyano/og_csv_files/LakeCast_data_digitized(2025).csv') 

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
  group_by(Date) %>% 
  filter(!is.na(FQ_PH) & !is.na(FQ_chl))

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

write.csv(talt_filled, 
          paste0('./data/talt_cyano/talt_cyano_formatted_', 
                 min(talt_filled$Date), '_',
                 max(talt_filled$Date), '.csv'),
          row.names = FALSE)

