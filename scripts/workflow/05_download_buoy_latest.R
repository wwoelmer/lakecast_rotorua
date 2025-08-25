# script to download latest buoy data

#devtools::install_github("karthik/rdrop2")
library(rdrop2)
library(dplyr)

# Read the token from GitHub Actions secret
token_string <- Sys.getenv("DROPBOX_APP_TOKEN")


# name of file once downloaded
local_file <- './data/buoy/rotorua_profiles_latest.csv'

# list the files in the rotorua folder and select the most recent one
fls <- drop_dir("bop/Rotorua/", dtoken = token_string)
fls <- fls %>% 
  arrange(desc(client_modified))
fl_name <- as.data.frame(fls[1,2])

# download the most recent file
rdrop2::drop_download(paste0("bop/Rotorua/", fl_name),
                      local_path = local_file,
                      overwrite = TRUE,
                      dtoken = token_string)


# now download meteorology
fl_name_met <- as.data.frame(fls[2,2])
# name of file once downloaded
local_file_met <- './data/buoy/rotorua_meteorology_latest.csv'

rdrop2::drop_download(paste0("bop/Rotorua/", fl_name_met),
                      local_path = local_file_met,
                      overwrite = TRUE,
                      dtoken = token_string)


#file.remove(".httr-oauth")
