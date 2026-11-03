# Relative Strength Index with Wilder's smoothing
rsi <- function(data, period) {
  n <- length(data)

  if (n <= period) {
    warning("Data length should be greater than the period")
    return(rep(NA_real_, n))
  }

  diff_values <- data[-1] - data[-n]
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  rsi_values <- rep(NA_real_, n)

  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }

  return(rsi_values)
}
