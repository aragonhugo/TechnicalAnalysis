# Moving Average Convergence Divergence
if (!exists("ema", mode = "function")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line

  result <- list(macd_line = macd_line,
                 signal_line = signal_line,
                 histogram = histogram)
  return(result)
}
