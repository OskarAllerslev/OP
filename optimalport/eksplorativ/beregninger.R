

library(quantmod)
library(tidyverse)
library(quadprog)
library(ggplot2)
library(reshape2)





load_data <- function(s,ticker_list){
  ticker_data <- list()
  for (ticker in ticker_list){
    ticker_data[[ticker]] <- quantmod::getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = s,
      auto.assign = FALSE
    )
  }
  return(ticker_data)
}




calculate_expected_return <- function(data){
  expected_returns <- list()
  #browser()
  for (ticker in names(data)){
    R <- quantmod::dailyReturn(
      quantmod::Ad(data[[ticker]]),
      type = "log"
    )

    ER <- mean(R, na.rm = TRUE)*252

    expected_returns[[ticker]] <- ER
  }
  return(expected_returns)
}




calculate_cov_matrix <- function(data_list){
  returns_list <- lapply(data_list, function(x) dailyReturn(Ad(x), type = "log"))
  returns_matrix <- do.call(cbind, returns_list)
  colnames(returns_matrix) <- names(data_list)

  cov_matrix <- cov(returns_matrix, use = "pairwise.complete.obs") * 252  #
  return(cov_matrix)
}



max_sharpe_portfolio <- function(expected_returns, cov_matrix, risk_free_rate = 0.02){
  n <- length(expected_returns)
  mu <- unlist(expected_returns)

  Dmat <- 2 * cov_matrix  # Kvadratisk led (kovarians)
  dvec <- rep(0, n)  # Ingen lineært led
  Amat <- rbind(1, diag(n))  # Constraints: sum(w) = 1, w_i >= 0
  bvec <- c(1, rep(0.02, n))  # Begrænsninger

  # Løs optimeringsproblemet
  res <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq = 1)

  # Hent de optimale vægte
  optimal_weights <- res$solution


  return(optimal_weights)
}


ticker_list <- c(
  "VWCE.DE",
  "EUNL.DE",
  "IWDA.AS",
  #"SPYD.L",
  "VUSA.L",

  #Emerging markets
  "VFEM.L",

  #japanske og asien
  "IJPA.AS",
  "CPXJ.AS",

  # kinesiske aktier

  "FXC.L",

  # sektor - og tematiske
  "VHYL.L",

  #obligationer
  "SGLO.L",
  "VAGU.L"

)

dat <- load_data(s = "2024-01-01", ticker_list)
benchmark <- load_data(s = "2024-01-01", c("^OMXC25"))

benchmark_ret <- calculate_expected_return(benchmark)
ret <- calculate_expected_return(dat)
cov <- calculate_cov_matrix(dat)


optimal <- max_sharpe_portfolio(
  expected_returns = ret,
  cov_matrix = calculate_cov_matrix(dat)
)


opti <- tibble(
  Ticker = names(ret),
  Expected_Return <- unlist(ret),
  Optimal_weight = round(optimal, 3)
)









create_portfolio_table <- function(ret, cov_matrix, optimal_weights, benchmark_ret) {
  # Beregn varians for hver aktiv
  variances <- diag(cov_matrix)

  # Konverterer til tibble
  portfolio_table <- tibble(
    Ticker = names(ret),
    Expected_Return = round(unlist(ret), 3),
    Variance = round(variances, 5),
    Optimal_Weight = round(optimal_weights, 3)
  )

  # Beregn porteføljens samlede afkast og varians
  portfolio_return <- sum(portfolio_table$Expected_Return * portfolio_table$Optimal_Weight)
  portfolio_variance <- sum(portfolio_table$Variance * portfolio_table$Optimal_Weight^2)

  # Tilføj portefølje total
  portfolio_table <- portfolio_table %>%
    add_row(Ticker = "Portfolio",
            Expected_Return = round(portfolio_return, 3),
            Variance = round(portfolio_variance, 5),
            Optimal_Weight = 1.0)

  # Tilføj benchmark (OMXC25)
  portfolio_table <- portfolio_table %>%
    add_row(Ticker = "OMXC25",
            Expected_Return = round(unlist(benchmark_ret), 3),
            Variance = NA,
            Optimal_Weight = 0)

  return(portfolio_table)
}

portfolio_table <- create_portfolio_table(ret, cov, optimal, benchmark_ret)




# Definer sharpe-funktionen (samme som for porteføljen)
sharpe <- function(Rp, Rf, Sp) {
  (Rp - Rf) / Sp
}

# --- Beregning for porteføljen (som du allerede har gjort) ---
# Konverter optimal til en kolonnevektor (hvis den ikke allerede er det)
optimal_mat <- matrix(optimal, ncol = 1)

# Beregn porteføljeafkastet (Rp)
portfolio_return <- as.numeric(t(optimal_mat) %*% unlist(ret))

# Beregn porteføljens standardafvigelse (Sp)
portfolio_sd <- sqrt(as.numeric(t(optimal_mat) %*% as.matrix(cov) %*% optimal_mat))

# Udregn porteføljens Sharpe-ratio
portfolio_sharpe <- sharpe(
  Rp = portfolio_return,
  Rf = 0.02,
  Sp = portfolio_sd
)

portfolio_sharpe

# --- Beregning for benchmark (OMXC25) ---

# Indlæs benchmark-data
benchmark <- load_data(s = "2024-01-01", c("^OMXC25"))

# Beregn benchmarkens årlige forventede afkast
# calculate_expected_return returnerer en liste, så vi udtrækker den første værdi
bench_ret <- calculate_expected_return(benchmark)[[1]]

# Beregn de daglige log-afkast for benchmarken
bench_daily <- dailyReturn(Ad(benchmark[[1]]), type = "log")

# Beregn benchmarkens årlige standardafvigelse (volatilitet)
bench_sd <- sqrt(var(bench_daily, na.rm = TRUE) * 252)

# Udregn benchmarkens Sharpe-ratio
benchmark_sharpe <- sharpe(
  Rp = bench_ret,
  Rf = 0.02,
  Sp = bench_sd
)

benchmark_sharpe


