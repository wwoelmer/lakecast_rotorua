# combine fixed sensor data into one dataframe
#install.packages('scoringRules')
library(scoringRules)
library(tidyverse)
'%notin%' <- Negate('%in%')

################################################################################
## put all files into the same dataframe

dir <- './data/buoy/historical_fixed_sensor_data/'

fls <- list.files(dir, pattern = '*.csv')
fls

dat <- NULL

for(i in 2:length(fls)){
  d <- read.csv(file.path(dir, fls[i]), skip = 14)
  colnames(d) <- c('datetime_UTC', 'datetime', 'value', 'approval_level', 'grade', 'qualifiers')
  
  var_name <- strsplit(fls[i], "[.]")[[1]][1]
  d$variable <- var_name
  print(var_name)
  d <- d %>% 
    select(datetime, value, variable)
  d$fl_name <- fls[i]
  
  if(var_name=='Water_Temp'){
    depth <- strsplit(fls[i], "[UoW_]")[[1]][7]
    depth <- strsplit(depth, 'm')[[1]][1]
    
    d$depth <- depth
    print(depth)
  }else if(var_name=='SpC'){
    depth <- strsplit(fls[i], "[UoW_]")[[1]][5]
    depth <- strsplit(depth, 'm')[[1]][1]
    
    d$depth <- depth
    print(depth)
  }else if(var_name=='DO_sat'){
    depth <- strsplit(fls[i], "[UoW_]")[[1]][6]
    depth <- strsplit(depth, 'm')[[1]][1]
    
    d$depth <- depth
    print(depth)
  }else{
    d$depth <- 1
    
  }
  
  dat <- rbind(dat, d)
}

# fis the 1m water temp which extracted the wrong depth
dat <- dat %>% 
  mutate(depth = ifelse(depth=='rd.csv', 1, depth))

dat$depth <- as.numeric(dat$depth)

write.csv(dat, paste0('./data/buoy/rotorua_fixed_sensor_data_', 
                      min(as.Date(dat$datetime)), '_', 
                      max(as.Date(dat$datetime)), '.csv'),
          row.names = FALSE)

