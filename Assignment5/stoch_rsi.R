# Stochastic RSI
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)

  if (all(is.na(rsi_values))) {
    min_rsi <- NA_real_
    max_rsi <- NA_real_
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
  }
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)

  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
