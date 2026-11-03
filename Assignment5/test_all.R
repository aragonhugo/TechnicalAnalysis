if (dir.exists("Assignment5")) setwd("Assignment5")

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi", "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

check <- function(name, ok) cat(if (isTRUE(ok)) "PASS" else "FAIL", "-", name, "\n")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

check("sma", isTRUE(all.equal(sma(data, 3),
  c(12.33333, 15.66667, 17.66667, 20, 21.66667, 23.66667, 23.33333), tolerance = 1e-5)))

check("ema", isTRUE(all.equal(ema(data, 3),
  c(10, 11, 13, 16.5, 17.25, 19.625, 22.3125, 23.15625, 22.078125))))

m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2)
check("macd histogram", isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))

check("stdev", isTRUE(all.equal(stdev(data), sqrt(var(data) * (length(data) - 1) / length(data)))))

line <- linreg(2 * (1:10), 10, 0)
check("linreg slope and intercept", isTRUE(all.equal(c(line$slope, line$intercept), c(2, 0))))

up <- rsi(1:20, 14)
check("rsi rising prices", all(is.na(up[1:14])) && all(up[15:20] == 100))
check("rsi range", {
  r <- rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 5)
  all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100)
})

set.seed(1)
s <- stoch_rsi(cumsum(rnorm(40)) + 100, 5, 3, 3)
check("stoch_rsi between 0 and 1", {
  k <- s$k_line[!is.na(s$k_line)]
  length(k) > 0 && all(k >= 0 & k <= 1)
})

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
check("crossover", which(crossover(arr1, arr2)) == 4)
check("crossunder", which(crossunder(arr2, arr1)) == 4)
check("crossunder none", !any(crossunder(arr1, arr2)))
