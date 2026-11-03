# TRUE where arr1 crosses under arr2
crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")

  crossunder_signals <- logical(length(arr1))

  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossunder_signals[i] <- TRUE
    }
  }

  return(crossunder_signals)
}
