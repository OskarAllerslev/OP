library(quantmod)
library(tidyverse)
library(reshape2)
library(xts)

plot_portfolio_performance <- function(asset_data, benchmark_data, optimal_weights) {

  # 1) Get daily arithmetic returns for each asset
  asset_returns_list <- lapply(asset_data, function(x) {
    dailyReturn(Ad(x), type = "arithmetic")
  })
  # Merge all asset returns into a single xts with each column = an asset
  asset_returns <- do.call(merge, asset_returns_list)
  colnames(asset_returns) <- names(asset_data)

  # 2) Get daily arithmetic returns for the benchmark
  #    (assuming benchmark_data is a named list of length 1)
  #    rename it to "Benchmark" right away
  benchmark_ticker <- names(benchmark_data)[1]
  benchmark_returns <- dailyReturn(Ad(benchmark_data[[1]]), type = "arithmetic")
  colnames(benchmark_returns) <- "Benchmark"

  # 3) Construct the portfolio's daily returns = weighted sum of assets
  #    Make sure we handle any NAs. A simple approach: fill NAs with zero.
  asset_returns_noNA <- na.fill(asset_returns, 0)
  portfolio_return_vec <- rowSums(asset_returns_noNA *
                                    matrix(optimal_weights,
                                           nrow = nrow(asset_returns_noNA),
                                           ncol = ncol(asset_returns_noNA),
                                           byrow = TRUE))
  portfolio_returns <- xts(portfolio_return_vec, order.by = index(asset_returns_noNA))
  colnames(portfolio_returns) <- "Portfolio"

  # 4) Build cumulative performance (start at 100).
  #    Using simple formula: cumprod(1 + daily_return) * 100
  asset_performance     <- cumprod(1 + asset_returns)     * 100
  benchmark_performance <- cumprod(1 + benchmark_returns) * 100
  portfolio_performance <- cumprod(1 + portfolio_returns) * 100

  # 5) Merge everything into one xts.
  #    At this point, each column in asset_performance is an asset,
  #    and we have "Benchmark" and "Portfolio" named columns.
  combined <- merge(asset_performance, benchmark_performance, portfolio_performance)

  # 6) Convert wide xts -> long data frame for ggplot
  df_long <- data.frame(Date = index(combined), coredata(combined))
  df_long <- reshape2::melt(df_long, id.vars = "Date",
                            variable.name = "Ticker",
                            value.name   = "Performance")

  # 7) Define color mapping:
  #    - "Benchmark" should be red
  #    - "Portfolio" should be green
  #    - Everything else (individual ETFs) can stay a default color
  all_tickers <- unique(df_long$Ticker)
  color_vector <- rep("gray40", length(all_tickers))  # default color
  names(color_vector) <- all_tickers

  if ("Benchmark"  %in% all_tickers) color_vector["Benchmark"]  <- "red"
  if ("Portfolio"  %in% all_tickers) color_vector["Portfolio"] <- "green"

  # 8) Plot
  ggplot(df_long, aes(x = Date, y = Performance, color = Ticker)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = color_vector) +
    labs(
      title = "Performance of Assets vs. Benchmark & Portfolio",
      x = "Date",
      y = "Index (Start = 100)"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

# Example usage:
  dat <- load_data("2024-01-01", ticker_list)
  benchmark <- load_data("2024-01-01", "^OMXC25")
  optimal   <- optimal  # your optimal weights
  plot_portfolio_performance(dat, benchmark, optimal)
