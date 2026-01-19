remotes::install_github("limnotrack/aemetools")
get_lake_wqprofiler <- function(lake = "rotorua",
                                api_url = "https://api.limnotrack.com", 
                                api_key = NULL) {
  
  res <- aemetools::api_request(api_url = api_url, api_key = api_key,
                                endpoint = "get_lake_wqprofiler", 
                                query = list(lake = lake))
  
  if (httr2::resp_status(res) != 200) {
    stop("API request failed with status: ", httr2::resp_status(res))
  }
  
  df <- res |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  
  return(df)
}

# example usage
#df <- get_lake_wqprofiler(lake = "rotorua", api_key = Sys.getenv("LERNZMP_KEY"))
#head(df)
