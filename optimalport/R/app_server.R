#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  library(shiny)
  library(quantmod)   # evt. for Ad() og dailyReturn(), men nu bliver de også importeret via din pakke
  library(tidyverse)


  # Indlæs alle pakke-funktioner
  devtools::load_all()

  portfolio_data <- reactive({
    req(input$start_date)

    # Indlæs data baseret på den valgte startdato
    dat <- load_data(s = as.character(input$start_date), ticker_list)
    benchmark <- load_data(s = as.character(input$start_date), c("^OMXC25"))

    # Beregn forventede afkast og kovarians for aktiverne
    ret <- calculate_expected_return(dat)
    cov <- calculate_cov_matrix(dat)
    optimal <- max_sharpe_portfolio(ret, cov)

    # Beregn porteføljens afkast og volatilitet med fuld kovarians (korrekt)
    optimal_mat <- matrix(optimal, ncol = 1)
    portfolio_return <- as.numeric(t(optimal_mat) %*% unlist(ret))
    portfolio_volatility <- sqrt(as.numeric(t(optimal_mat) %*% as.matrix(cov) %*% optimal_mat))

    # Beregn benchmark-afkast og -volatilitet
    benchmark_ret_list <- calculate_expected_return(benchmark)
    bench_ret <- as.numeric(benchmark_ret_list[[1]])
    bench_daily <- dailyReturn(Ad(benchmark[[1]]), type = "log")
    bench_volatility <- sqrt(var(bench_daily, na.rm = TRUE) * 252)

    # Opret portefølje-tabel (her skal vi bemærke, at porteføljevariansen i tabellen er en forenklet udregning)
    portfolio_table <- create_portfolio_table(ret, cov, optimal, benchmark_ret_list)

    list(
      table = portfolio_table,
      portfolio_return = portfolio_return,
      portfolio_volatility = portfolio_volatility,
      bench_ret = bench_ret,
      bench_volatility = bench_volatility
    )
  })

  # Output: Portefølje-tabel
  output$portfolio_table <- renderTable({
    portfolio_data()$table
  })

  # Definer sharpe-funktionen
  sharpe <- function(Rp, Rf, Sp) {
    (Rp - Rf) / Sp
  }

  # Output: Sharpe Ratio sammenligning
  output$sharpe_table <- renderTable({
    data <- portfolio_data()
    risk_free_rate <- 0.02

    sharpe_portfolio <- sharpe(
      Rp = data$portfolio_return,
      Rf = risk_free_rate,
      Sp = data$portfolio_volatility
    )

    sharpe_benchmark <- sharpe(
      Rp = data$bench_ret,
      Rf = risk_free_rate,
      Sp = data$bench_volatility
    )

    tibble(
      Måling = c("Sharpe Ratio - Portefølje", "Sharpe Ratio - OMXC25"),
      Værdi  = c(round(sharpe_portfolio, 3), round(sharpe_benchmark, 3))
    )
  })
}
