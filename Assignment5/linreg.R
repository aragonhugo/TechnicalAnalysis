# Linear Regression
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)

  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  if (start_index > end_index) stop("regressionOffset is too large for regressionLength")

  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)

  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)

  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept

  result <- list(slope = slope,
                 intercept = intercept,
                 predicted_values = predicted_values)
  return(result)
}
