library(quantmod)
library(TTR)

load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != ""]
  stocks <- list()

  for (s in symbols) {
    x <- tryCatch(
      getSymbols(s, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        message("Could not load ", s)
        NULL
      }
    )
    if (is.null(x) || nrow(x) == 0) next

    df <- data.frame(Date = index(x), coredata(x))
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[s]] <- na.omit(df)
  }

  if (length(stocks) == 0) stop("No data loaded. Check your internet and portfolio.txt.")
  stocks
}

get_mode <- function(x) {
  x <- round(x)
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

calculate_statistics <- function(df, window = 20) {
  close <- df$Close
  ma <- SMA(close, n = window)
  list(
    summary = data.frame(
      Mean = round(mean(close), 2),
      Median = round(median(close), 2),
      Mode = get_mode(close),
      SD = round(sd(close), 2),
      Latest_MA = round(tail(ma, 1), 2)
    ),
    ma = ma
  )
}

all_statistics <- function(stocks, window = 20) {
  stats <- lapply(stocks, calculate_statistics, window = window)
  rows <- lapply(names(stats), function(s) cbind(Symbol = s, stats[[s]]$summary))
  do.call(rbind, rows)
}

show_data <- function(stocks, n = 5) {
  for (s in names(stocks)) {
    cat("\n\n**", s, "**\n\n", sep = "")
    print(knitr::kable(tail(stocks[[s]], n), row.names = FALSE))
  }
}

plot_stock <- function(df, symbol, ma) {
  plot(df$Date, df$Close, type = "l", main = symbol,
       xlab = "Date", ylab = "Close price")
  lines(df$Date, ma, col = "red", lwd = 2)
  legend("topleft", legend = c("Close", "Moving average"),
         col = c("black", "red"), lty = 1, bty = "n")
}
