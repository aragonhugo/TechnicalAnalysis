# Standard Deviation, dividing by n as in the formula
stdev <- function(data) {
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
