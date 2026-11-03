# Portfolio dashboard: prices, indicators and trading signals
library(shiny)
library(ggplot2)
library(quantmod)

# Data -------------------------------------------------------------
symbols <- if (file.exists("portfolio.txt")) trimws(readLines("portfolio.txt", warn = FALSE)) else character(0)
symbols <- symbols[symbols != ""]
if (length(symbols) == 0) symbols <- c("RY.TO", "TD.TO", "ENB.TO", "SHOP.TO", "AAPL")

fetch_prices <- function(symbol) {
  x <- getSymbols(symbol, src = "yahoo", from = "2015-01-01",
                  to = Sys.Date() + 1, auto.assign = FALSE)
  df <- data.frame(Date = index(x), coredata(x))
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  na.omit(df)
}

resample <- function(df, frame) {
  if (frame == "Daily") return(df)
  key <- if (frame == "Weekly") format(df$Date, "%G-%V") else format(df$Date, "%Y-%m")
  groups <- split(df, factor(key, levels = unique(key)))
  out <- do.call(rbind, lapply(groups, function(g) {
    data.frame(Date = max(g$Date), Open = g$Open[1], High = max(g$High),
               Low = min(g$Low), Close = g$Close[nrow(g)],
               Volume = sum(g$Volume), Adjusted = g$Adjusted[nrow(g)])
  }))
  rownames(out) <- NULL
  out
}

# Indicators and trading rule ----------------------------------------
safe <- function(expr, n) tryCatch(expr, error = function(e) rep(NA_real_, n))

add_indicators <- function(df, short_n, long_n, rsi_n) {
  n <- nrow(df)
  df$MA_short <- safe(as.numeric(SMA(df$Close, short_n)), n)
  df$MA_long <- safe(as.numeric(SMA(df$Close, long_n)), n)
  df$RSI <- safe(as.numeric(RSI(df$Close, rsi_n)), n)
  m <- safe(MACD(df$Close, 12, 26, 9), n)
  if (is.matrix(m)) {
    df$MACD <- as.numeric(m[, "macd"])
    df$MACD_signal <- as.numeric(m[, "signal"])
  } else {
    df$MACD <- NA_real_
    df$MACD_signal <- NA_real_
  }
  df$MACD_hist <- df$MACD - df$MACD_signal
  df
}

# Buy when the short MA crosses above the long MA, Sell when it crosses below
add_signals <- function(df) {
  above <- df$MA_short > df$MA_long
  prev <- c(NA, above[-length(above)])
  ok <- !is.na(above) & !is.na(prev)
  df$Signal <- "Hold"
  df$Signal[ok & above & !prev] <- "Buy"
  df$Signal[ok & !above & prev] <- "Sell"
  df
}

# Plots ----------------------------------------------------------------
price_plot <- function(df, chart_type, show_ma, show_signals, show_hold, short_n, long_n) {
  green <- "#2e7d32"
  red <- "#c62828"
  p <- ggplot(df, aes(x = Date))

  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close), colour = "grey30")
  } else if (chart_type == "Area") {
    p <- p + geom_area(aes(y = Close), fill = "steelblue", alpha = 0.3) +
      geom_line(aes(y = Close), colour = "steelblue")
  } else {
    df$Up <- df$Close >= df$Open
    w <- max(0.4, min(3, 250 / nrow(df)))
    p <- ggplot(df, aes(x = Date)) +
      geom_linerange(aes(ymin = Low, ymax = High, colour = Up)) +
      geom_linerange(aes(ymin = pmin(Open, Close), ymax = pmax(Open, Close), colour = Up),
                     linewidth = w) +
      scale_colour_manual(values = c("TRUE" = green, "FALSE" = red), guide = "none")
  }

  subtitle <- NULL
  if (show_ma) {
    p <- p +
      geom_line(aes(y = MA_short), colour = "#1565c0", linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MA_long), colour = "#ef6c00", linewidth = 0.8, na.rm = TRUE)
    subtitle <- paste0("Blue: ", short_n, " period MA. Orange: ", long_n, " period MA.")
  }

  rng <- range(c(df$Low, df$High, df$Close), na.rm = TRUE)
  gap <- 0.04 * diff(rng)

  if (show_signals) {
    buy <- df[df$Signal == "Buy", ]
    sell <- df[df$Signal == "Sell", ]
    p <- p +
      geom_point(data = buy, aes(y = Low - gap), shape = 24, fill = green,
                 colour = green, size = 3) +
      geom_text(data = buy, aes(y = Low - gap, label = "Buy"), colour = green,
                fontface = "bold", vjust = 2) +
      geom_point(data = sell, aes(y = High + gap), shape = 25, fill = red,
                 colour = red, size = 3) +
      geom_text(data = sell, aes(y = High + gap, label = "Sell"), colour = red,
                fontface = "bold", vjust = -1.2)
    if (show_hold) {
      hold <- df[df$Signal == "Hold", ]
      p <- p + geom_text(data = hold, aes(y = Close, label = "Hold"),
                         colour = "grey50", size = 2.5, vjust = 1.5)
    }
  }

  pad <- 0.12 * diff(rng)
  p + coord_cartesian(ylim = c(rng[1] - pad, rng[2] + pad)) +
    labs(x = NULL, y = "Price", subtitle = subtitle) +
    theme_minimal()
}

rsi_plot <- function(df) {
  ggplot(df, aes(Date, RSI)) +
    geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "grey50") +
    geom_line(colour = "#6a1b9a", na.rm = TRUE) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = NULL, y = "RSI", subtitle = "Above 70 is overbought, below 30 is oversold.") +
    theme_minimal()
}

macd_plot <- function(df) {
  ggplot(df, aes(Date)) +
    geom_col(aes(y = MACD_hist), fill = "grey70", na.rm = TRUE) +
    geom_line(aes(y = MACD), colour = "#1565c0", na.rm = TRUE) +
    geom_line(aes(y = MACD_signal), colour = "#ef6c00", na.rm = TRUE) +
    labs(x = NULL, y = "MACD", subtitle = "Blue: MACD line. Orange: signal line. Bars: histogram.") +
    theme_minimal()
}

# App ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Portfolio Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Stock:", choices = symbols),
      dateRangeInput("dates", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:", choices = c("Candlestick", "Line", "Area")),
      checkboxGroupInput("indicators", "Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      numericInput("short_ma", "Short MA period:", value = 20, min = 2),
      numericInput("long_ma", "Long MA period:", value = 50, min = 3),
      numericInput("rsi_n", "RSI period:", value = 14, min = 2),
      checkboxInput("show_signals", "Show Buy and Sell signals", value = TRUE),
      checkboxInput("show_hold", "Also label Hold bars", value = FALSE)
    ),
    mainPanel(
      plotOutput("price_chart", height = "420px"),
      conditionalPanel("input.indicators.indexOf('RSI') > -1",
                       plotOutput("rsi_chart", height = "200px")),
      conditionalPanel("input.indicators.indexOf('MACD') > -1",
                       plotOutput("macd_chart", height = "200px")),
      h4("Latest Buy and Sell signals"),
      tableOutput("signal_table")
    )
  )
)

server <- function(input, output, session) {
  prices <- reactive({
    req(input$symbol)
    withProgress(message = "Loading prices", {
      tryCatch(fetch_prices(input$symbol), error = function(e) {
        showNotification(paste0("Could not load ", input$symbol, ". Check your internet."),
                         type = "error")
        NULL
      })
    })
  })

  view <- reactive({
    req(input$short_ma, input$long_ma, input$rsi_n, input$dates)
    validate(need(!is.null(prices()), "No data to show. Try another stock."),
             need(input$short_ma < input$long_ma, "The short MA must be smaller than the long MA."),
             need(input$dates[1] < input$dates[2], "The start date must be before the end date."))
    df <- resample(prices(), input$time_frame)
    df <- add_signals(add_indicators(df, input$short_ma, input$long_ma, input$rsi_n))
    df <- df[df$Date >= input$dates[1] & df$Date <= input$dates[2], ]
    validate(need(nrow(df) > 1, "No data in this date range."))
    df
  })

  output$price_chart <- renderPlot({
    price_plot(view(), input$chart_type, "Moving Averages" %in% input$indicators,
               input$show_signals, input$show_hold, input$short_ma, input$long_ma)
  })
  output$rsi_chart <- renderPlot(rsi_plot(view()))
  output$macd_chart <- renderPlot(macd_plot(view()))

  output$signal_table <- renderTable({
    df <- view()
    s <- df[df$Signal != "Hold", ]
    validate(need(nrow(s) > 0, "No Buy or Sell signals in this range."))
    s <- head(s[order(s$Date, decreasing = TRUE), ], 10)
    data.frame(Date = format(s$Date), Signal = s$Signal, Close = round(s$Close, 2))
  })
}

shinyApp(ui, server)
