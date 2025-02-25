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
#' Henter kursdata for en række tickers fra Yahoo.
#' @param s Startdato (fx "2024-01-01").
#' @param ticker_list En karaktervektor af ticker-strenge.
#' @return En liste med xts-objekter indeholdende prisdata.
load_data <- function(s, ticker_list) {
  ticker_data <- list()
  for (ticker in ticker_list) {
    ticker_data[[ticker]] <- quantmod::getSymbols(
      Symbols    = ticker,
      src        = "yahoo",
      from       = s,
      auto.assign= FALSE
    )
  }
  return(ticker_data)
}

#' Beregn forventet årligt afkast ud fra daglige log-afkast.
#'
#' @param data En liste af xts-objekter.
#' @return En navngivet liste med forventede årlige afkast.
calculate_expected_return <- function(data) {
  expected_returns <- list()

  for (ticker in names(data)) {
    R <- quantmod::dailyReturn(
      quantmod::Ad(data[[ticker]]),
      type = "log"
    )
    R <- na.omit(R)  # fjern NA'er hvis de findes

    # Antal handelsdage i datasættet
    n_days <- length(R)

    # Annualiseret afkast (gange med n_days)
    ER <- mean(R, na.rm = TRUE) * n_days

    expected_returns[[ticker]] <- ER
  }
  return(expected_returns)
}

#' Beregn årlig kovariansmatrix ud fra daglige log-afkast.
#'
#' Filtrer aktiver med varians under en vis grænse (threshold).
#'
#' @param data_list En liste af xts-objekter.
#' @param threshold Numerisk grænse for minimumsvarians (default 1e-2).
#' @return Den filtrerede annualiserede kovariansmatrix.
calculate_cov_matrix <- function(data_list, threshold = 1e-3) {
  # Omdan hver xts til en serie af log-afkast, fjern NA
  returns_list <- lapply(data_list, function(x) {
    na.omit(quantmod::dailyReturn(quantmod::Ad(x), type = "log"))
  })

  # Saml til en stor matrix
  returns_matrix <- do.call(cbind, returns_list)
  colnames(returns_matrix) <- names(data_list)

  # Kovarians * 252 for at annualisere
  cov_matrix <- stats::cov(returns_matrix, use = "pairwise.complete.obs") * 252

  # Beregn varians pr. aktiv
  variances <- diag(cov_matrix)

  # Fjern aktiver med næsten nul varians
  valid_assets <- names(variances[variances > threshold])

  # Hvis der kun er få aktiver tilbage => stop
  if (length(valid_assets) < 2) {
    stop("For få aktiver med tilstrækkelig varians tilbage efter filtrering!")
  }

  # Returnér kun den filtrerede del
  cov_matrix_filtered <- cov_matrix[valid_assets, valid_assets, drop = FALSE]

  return(cov_matrix_filtered)
}

#' Find den optimale portefølje (maximér Sharpe Ratio).
#'
#' Indeholder også constraints:
#' - sum(weights) = 1 (long-only)
#' - weights >= 0
#' - weights <= 0.30 (som eksempel på max 30% i ét aktiv)
#'
#' @param expected_returns En liste med forventede afkast (samme aktiver som cov_matrix).
#' @param cov_matrix Den annualiserede kovariansmatrix (filtreret).
#' @param risk_free_rate Numerisk (default 0.02).
#' @return En vektor med optimale vægte.
max_sharpe_portfolio <- function(expected_returns, cov_matrix, risk_free_rate = 0.02) {
  # Match kun de aktiver, der er i kolonnerne af cov_matrix
  valid_assets    <- colnames(cov_matrix)
  filtered_returns <- expected_returns[valid_assets]

  n <- length(filtered_returns)
  if (n < 2) {
    stop("For få aktiver til at lave optimering!")
  }

  mu   <- unlist(filtered_returns)
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n)

  # Constraint: sum(w) = 1 (ligger i første række)
  # weights >= 0         (ligger i diag(n))
  # weights <= 0.30 => -weights >= -0.30
  max_weight <- 0.15
  Amat <- rbind(
    rep(1, n),  # sum(w)
    diag(n),    # w >= 0
    -diag(n)    # -w >= -0.30
  )
  bvec <- c(
    1,
    rep(0, n),
    rep(-max_weight, n)
  )
  meq <- 1  # kun sum(w)=1 er equality

  # Løs QP
  res <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq = meq)

  optimal_weights <- res$solution
  return(optimal_weights)
}

#' Opret en tabel med porteføljeinformation
#'
#' Retter sig kun mod de aktiver, der er i cov_matrix (filtreret).
#'
#' @param ret En liste med forventede afkast (indeholder mindst de aktiver i cov_matrix).
#' @param cov_matrix Den annualiserede kovariansmatrix (filtreret).
#' @param optimal_weights Vektor med vægte (samme rækkefølge som cov_matrix).
#' @param benchmark_ret En liste (eller enkeltværdi) med benchmark-afkast.
#' @return Et tibble med aktiver + rækker for portefølje og benchmark.
create_portfolio_table <- function(ret, cov_matrix, optimal_weights, benchmark_ret) {
  # Filtrér ret, så kun aktiver i kovariansmatricen tages med
  assets_in_cov <- colnames(cov_matrix)
  ret           <- ret[assets_in_cov]

  # Varians for aktiverne (kun diagonalen)
  variances <- diag(cov_matrix)

  # Opret tabellen for de valgte aktiver
  portfolio_table <- tibble::tibble(
    Ticker          = assets_in_cov,
    Expected_Return = round(unlist(ret), 3),
    Variance        = round(variances,    5),
    Optimal_Weight  = round(optimal_weights, 3)
  )

  # Beregn porteføljens samlede afkast og varians
  portfolio_return   <- sum(portfolio_table$Expected_Return * portfolio_table$Optimal_Weight)
  portfolio_variance <- sum(portfolio_table$Variance         * (portfolio_table$Optimal_Weight^2))

  # Tilføj en ekstra række for den samlede portefølje
  portfolio_table <- portfolio_table %>%
    dplyr::add_row(
      Ticker          = "Portfolio",
      Expected_Return = round(portfolio_return,   3),
      Variance        = round(portfolio_variance, 5),
      Optimal_Weight  = 1.0
    ) %>%
    # Tilføj en række for benchmark (OMXC25), hvis du har beregnet dens afkast andetsteds
    dplyr::add_row(
      Ticker          = "OMXC25",
      Expected_Return = round(unlist(benchmark_ret), 3), # Evt. 0, hvis du ikke har data
      Variance        = NA,
      Optimal_Weight  = 0
    )

  return(portfolio_table)
}

#' Beregn Sharpe Ratio
#'
#' (Rp - Rf) / Sp
#' @param Rp Porteføljeafkast.
#' @param Rf Risikofri rente.
#' @param Sp Porteføljens standardafvigelse.
#' @return Sharpe Ratio (numerisk).
sharpe <- function(Rp, Rf, Sp) {
  (Rp - Rf) / Sp
}

#' Standard tickers for porteføljen
#'
#' ^OMXC25 er IKKE med her, da vi kun vil bruge den som benchmark (ikke i selve optimeringen).
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
  #"IEMB.AS", # Emerging Markets Bonds

  # Japanske og asiatiske aktier
  "IJPA.AS",  # MSCI Japan
  "CPXJ.AS",  # MSCI Pacific Ex Japan
  "FXC.L",    # MSCI China

  # Sektorbaserede og High Dividend ETF'er
  "VHYL.L",   # High Dividend Yield
  "ISPY.L",   # Global Infrastructure
  "EXX1.DE",  # MSCI Europe ESG
  #"EFA.AS",  # MSCI EAFE

  # Obligationer og Fixed Income
  # "SGLO.L",   # Global Bonds Hedged EUR
  # "VAGU.L",   # US Aggregate Bonds

  # Råvarer og alternative investeringer
  "PHGP.L",   # Physical Gold
  "SPY4.L",   # Commodities Broad Market
  "GDX.L" ,    # Gold Miners ETF
  #"ICLN.AS"  # Clean Energy
  # Globale aktie-ETF'er
  "VWCE.DE",  # MSCI ACWI
  "EUNL.DE",  # MSCI World
  "IWDA.AS",  # MSCI World USD
  "VUSA.L",   # S&P 500
  "VWRL.AS",  # FTSE All-World

  # Emerging Markets
  "VFEM.L",   # FTSE Emerging Markets
  "IEMA.AS",  # MSCI Emerging Markets

  # Japanske og asiatiske aktier
  "IJPA.AS",  # MSCI Japan
  "CPXJ.AS",  # MSCI Pacific Ex Japan
  "FXC.L",    # MSCI China

  # Sektorbaserede og High Dividend ETF'er
  "VHYL.L",   # High Dividend Yield
  "ISPY.L",   # Global Infrastructure
  "EXX1.DE",  # MSCI Europe ESG
  # "ESEB.DE",  # MSCI Europe ESG Enhanced

  # Obligationer og Fixed Income
  # "SGLO.DE",  # Global Bonds Hedged EUR

  "NVO",
  # "FXEU.L",
  # "FXUS.L",
  # "FXGB.L",
  "SAP.DE",
  "OR.PA",
  "NESN.SW",
  # Råvarer og alternative investeringer
  "PHGP.L",   # Physical Gold
  "SPY4.L",   # Commodities Broad Market
  "GDX.L"    # Gold Miners ETF
  # "IGLN.DE"   # Physical Gold ETC
)
