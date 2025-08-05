devtools::install_github("karthik/rdrop2")
library(rdrop2)
drop_auth()

token <- drop_auth()
saveRDS(token, file = "token.rds")

# name of file once downloaded
local_file <- './data/buoy/rotorua_profiles_latest.csv'

# list the files in the rotorua folder and select the most recent one
fls <- drop_dir("bop/Rotorua/")
fls <- fls %>% 
  arrange(desc(client_modified))
fl_name <- as.data.frame(fls[1,2])

# download the most recent file
rdrop2::drop_download(paste0("bop/Rotorua/", fl_name),
                      local_path = local_file,
                      overwrite = TRUE)

