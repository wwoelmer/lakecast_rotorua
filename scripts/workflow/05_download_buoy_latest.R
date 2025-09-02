# script to download latest buoy data

#devtools::install_github("karthik/rdrop2")
library(rdrop2)
library(dplyr)
library(stringr)

# Read the token from GitHub Actions secret
token <- readRDS('token.rds')


# name of file once downloaded
local_file <- './data/buoy/rotorua_profiles_latest.csv'

# list the files in the rotorua folder and select the most recent one
fls <- drop_dir("bop/Rotorua/", dtoken = token)

fl_name <- fls %>% 
  arrange(desc(client_modified)) %>% 
  filter(str_detect(name, "profiles\\.csv$")) %>% 
  slice(1)

fl_name <- fl_name[1,2]

# download the most recent file
rdrop2::drop_download(paste0("bop/Rotorua/", fl_name),
                      local_path = local_file,
                      overwrite = TRUE,
                      dtoken = token)


# now download meteorology
fl_name_met <- fls %>% 
  arrange(desc(client_modified)) %>% 
  filter(str_detect(name, "meteorology\\.csv$")) %>% 
  slice(1)

fl_name_met <- fl_name_met[1,2]

# name of file once downloaded
local_file_met <- './data/buoy/rotorua_meteorology_latest.csv'

rdrop2::drop_download(paste0("bop/Rotorua/", fl_name_met),
                      local_path = local_file_met,
                      overwrite = TRUE,
                      dtoken = token)


#file.remove(".httr-oauth")
