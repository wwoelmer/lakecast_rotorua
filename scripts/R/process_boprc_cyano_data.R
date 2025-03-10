# routine processing and appending function for BOPRC cyanobacterial monitoring data
library(readxl)

#1: convert and form excel, save as csv
#2 append to existing data

dir <- 'data/boprc_cyano/og_excel_files'
fls <- list.files(dir)

fl_1 <- read_excel(paste0(dir, "/", fls[1])) %>% 
  filter(Location=='Rotorua') %>% 
  select(-SampleId)

for(i in 2:length(fls)){
  fl_i <- read_excel(paste0(dir, "/", fls[i])) %>% 
    filter(Location=='Rotorua') %>% 
    select(-SampleId)
  
  fl_1 <- full_join(fl_1, fl_i)
}

fl_1$SampleDate <- as.Date(fl_1$SampleDate, format = "%d/%m/%Y")

ggplot(fl_1, aes(x = as.Date(SampleDate), y = TotalBioVolume, color = Species)) +
  geom_point() +
  facet_wrap(~Site)

hist <- read.csv('./data/boprc_cyano/og_csv/Cyanobacteria_Data_Whitney_Dec_2024.csv') %>% 
  filter(Location=='Rotorua') %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  select(-SampleId)

fl_2 <- full_join(hist, fl_1)

# separate genus and species
fl_2 <- fl_2 %>% 
  separate(Species, into = c('genus', 'species'), sep = " ")

fl_2 %>% 
  filter(genus %in% c('Aphanocapsa', 'Dolichospermum', 'Microcystis', 'Pseudanabaena')) %>% 
  ggplot(aes(x = as.Date(SampleDate), y = TotalBioVolume, color = genus)) +
  geom_point() +
  facet_wrap(~Site, scales = 'free')

fl_2 %>% 
  filter(genus %in% c('Aphanocapsa', 'Dolichospermum', 'Microcystis', 'Pseudanabaena')) %>% 
  ggplot(aes(x = as.Date(SampleDate), y = CellsPerMl, color = genus)) +
  geom_point() +
  facet_wrap(~Site) 

colnames(fl_2)

write.csv(fl_2,
          paste0('./data/boprc_cyano/boprc_cyano_', 
                 min(fl_2$SampleDate), '_',
                 max(fl_2$SampleDate), 'csv'),
          row.names = FALSE)
