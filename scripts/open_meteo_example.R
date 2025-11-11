# download open meteo
devtools::install_github("FLARE-forecast/ropenmeteo")

library(ggplot2)
library(dplyr)
library(purrr)
library(tibble)
library(ropenmeteo)
library(lubridate)
library(readr)

# The open-meteo project combines the the best models for each location across the globe to provide the best possible forecast.  open-meteo defines this as `model = "generic"`.
# [https://open-meteo.com/en/docs]

df <- get_forecast(latitude = -38.078531955149366,
                   longitude = 176.26140967390774,
                   forecast_days = 7, 
                   past_days = 2, 
                   model = "generic",
                   variables = c("temperature_2m"))
head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line(color = "#F8766D") + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free")

# ensemble weather forecasts
# [https://open-meteo.com/en/docs/ensemble-api]
df <- get_ensemble_forecast(
  latitude = -38.078531955149366,
  longitude = 176.26140967390774,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = c("temperature_2m"))
head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

# Options for models and variables are at https://open-meteo.com/en/docs/ensemble-api

df <- get_ensemble_forecast(
  latitude = -38.078531955149366,
  longitude = 176.26140967390774,
  forecast_days = 7,
  past_days = 2,
  model = "gfs_seamless",
  variables = glm_variables(product = "ensemble_forecast", 
                            time_step = "hourly"))
head(df)

df |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

df <- df |> 
  add_longwave()

unique(df$variable)
# air temperature, shortwave radiation, windspeed, wind direction, air pressure
# get GLM variables
