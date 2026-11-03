# TRUE where arr1 crosses above arr2
crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")

  crossover_signals <- logical(length(arr1))

  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
      crossover_signals[i] <- TRUE
    }
  }

  return(crossover_signals)
}
