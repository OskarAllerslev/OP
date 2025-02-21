#' Portfolio Functions
#'
#' Denne fil indeholder funktioner til at indlæse data, beregne forventede afkast,
#' kovarians, optimere porteføljen (maximér Sharpe Ratio), lave en porteføljetabel samt beregne Sharpe Ratio.
#' @importFrom magrittr %>%
#' @importFrom quantmod getSymbols dailyReturn Ad
#' @importFrom quadprog solve.QP
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom stats cov var mean
NULL

#' Load data from Yahoo Finance
#'
#' @param s Startdato (character), fx "2024-01-01".
#' @param ticker_list En vektor af ticker-strenge.
#' @return En liste med xts-objekter indeholdende prisdata.
load_data <- function(s, ticker_list) {
  ticker_data <- list()
  for (ticker in ticker_list) {
    ticker_data[[ticker]] <- quantmod::getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = s,
      auto.assign = FALSE
    )
  }
  return(ticker_data)
}

#' Beregn forventet årligt afkast ud fra daglige log-afkast.
#'
#' @param data En liste af xts-objekter.
#' @return En liste med forventede årlige afkast.
calculate_expected_return <- function(data){
  expected_returns <- list()

  for (ticker in names(data)){
    R <- quantmod::dailyReturn(
      quantmod::Ad(data[[ticker]]),
      type = "log"
    )

    # Antal handelsdage i datasættet
    n_days <- sum(!is.na(R))

    # Annualiseret afkast baseret på faktiske handelsdage
    ER <- mean(R, na.rm = TRUE) * n_days

    expected_returns[[ticker]] <- ER
  }
  return(expected_returns)
}


#' Beregn årlig kovariansmatrix ud fra daglige log-afkast.
#'
#' @param data_list En liste af xts-objekter.
#' @return Den annualiserede kovariansmatrix.
calculate_cov_matrix <- function(data_list) {
  returns_list <- lapply(data_list, function(x) {
    quantmod::dailyReturn(quantmod::Ad(x), type = "log")
  })
  returns_matrix <- do.call(cbind, returns_list)
  colnames(returns_matrix) <- names(data_list)
  cov_matrix <- stats::cov(returns_matrix, use = "pairwise.complete.obs") * 252
  return(cov_matrix)
}

#' Find den optimale portefølje (maximér Sharpe Ratio)
#'
#' @param expected_returns En liste med forventede afkast.
#' @param cov_matrix Den annualiserede kovariansmatrix.
#' @param risk_free_rate Den risikofrie rente (default = 0.02).
#' @return En vektor med optimale vægte.
max_sharpe_portfolio <- function(expected_returns, cov_matrix, risk_free_rate = 0.02) {
  n <- length(expected_returns)
  mu <- unlist(expected_returns)
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n)
  Amat <- rbind(1, diag(n))
  bvec <- c(1, rep(0.02, n))
  res <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq = 1)
  optimal_weights <- res$solution
  return(optimal_weights)
}

#' Opret en tabel med porteføljeinformation
#'
#' @param ret En liste med forventede afkast.
#' @param cov_matrix Den annualiserede kovariansmatrix.
#' @param optimal_weights Den optimale vægtvektor.
#' @param benchmark_ret En liste med benchmark-afkast.
#' @return Et tibble med aktiver samt ekstra rækker for portefølje og benchmark.
create_portfolio_table <- function(ret, cov_matrix, optimal_weights, benchmark_ret) {
  # Beregn varians for hvert aktiv
  variances <- diag(cov_matrix)

  portfolio_table <- tibble::tibble(
    Ticker = names(ret),
    Expected_Return = round(unlist(ret), 3),
    Variance = round(variances, 5),
    Optimal_Weight = round(optimal_weights, 3)
  )

  # Beregn porteføljens samlede afkast og en forenklet varians (kun diagonaleled)
  portfolio_return <- sum(portfolio_table$Expected_Return * portfolio_table$Optimal_Weight)
  portfolio_variance <- sum(portfolio_table$Variance * portfolio_table$Optimal_Weight^2)

  portfolio_table <- portfolio_table %>%
    dplyr::add_row(Ticker = "Portfolio",
                   Expected_Return = round(portfolio_return, 3),
                   Variance = round(portfolio_variance, 5),
                   Optimal_Weight = 1.0) %>%
    dplyr::add_row(Ticker = "OMXC25",
                   Expected_Return = round(unlist(benchmark_ret), 3),
                   Variance = NA,
                   Optimal_Weight = 0)

  return(portfolio_table)
}

#' Beregn Sharpe Ratio
#'
#' @param Rp Porteføljeafkast.
#' @param Rf Risikofri rente.
#' @param Sp Porteføljens standardafvigelse.
#' @return Sharpe Ratio.
sharpe <- function(Rp, Rf, Sp) {
  (Rp - Rf) / Sp
}

#' Standard tickers for porteføljen
#'
#' @return En karaktervektor med ticker-strenge.
ticker_list <- c(
  # Globale aktie-ETF'er
  "VWCE.DE",  # MSCI ACWI
  "EUNL.DE",  # MSCI World
  "IWDA.AS",  # MSCI World USD
  "VUSA.L",   # S&P 500

  # Emerging Markets
  "VFEM.L",   # FTSE Emerging Markets
  #"IEMB.AS",  # Emerging Markets Bonds

  # Japanske og asiatiske aktier
  "IJPA.AS",  # MSCI Japan
  "CPXJ.AS",  # MSCI Pacific Ex Japan
  "FXC.L",    # MSCI China

  # Sektorbaserede og High Dividend ETF'er
  "VHYL.L",   # High Dividend Yield
  "ISPY.L",   # Global Infrastructure
  "EXX1.DE",  # MSCI Europe ESG
  #"EFA.AS",   # MSCI EAFE (Europa, Australien, Fjernøsten)

  # Obligationer og Fixed Income
  "SGLO.L",   # Global Bonds Hedged EUR
  "VAGU.L",   # US Aggregate Bonds
  "IBGS.L",   # Inflation-linked Bonds
  #"IDTP.AS",  # Euro Government Bonds
  #"IUSG.L",   # Investment Grade Corporate Bonds

  # Råvarer og alternative investeringer
  "PHGP.L",   # Physical Gold
  "SPY4.L",   # Commodities Broad Market
  "GDX.L"    # Gold Miners ETF
  #"ICLN.AS"   # Clean Energy ETF
)

