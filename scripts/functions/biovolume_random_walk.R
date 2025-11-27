library(ggplot2)

rw_forecast_log <- function(x, horizon = 10, level = 0.95) {
  # x must be positive and numeric
  x_log <- log(x + 1)
  diffs <- diff(x_log)
  sd_diff <- sd(diffs, na.rm = TRUE)
  last_val <- tail(x_log, 1)
  
  # Random walk mean and uncertainty on log scale
  mean_forecast <- rep(last_val, horizon)
  se <- sd_diff * sqrt(1:horizon)
  z <- qnorm((1 + level) / 2)
  
  lower_log <- mean_forecast - z * se
  upper_log <- mean_forecast + z * se
  
  # Back-transform to original scale
  data.frame(
    horizon = 1:horizon,
    mean = exp(mean_forecast - 1),
    lower = exp(lower_log - 1),
    upper = exp(upper_log - 1),
    last_obs = exp(last_val - 1)
  )
}
