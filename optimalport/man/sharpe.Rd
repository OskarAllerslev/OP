% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portfolio_functions.R
\name{sharpe}
\alias{sharpe}
\title{Beregn Sharpe Ratio}
\usage{
sharpe(Rp, Rf, Sp)
}
\arguments{
\item{Rp}{Porteføljeafkast.}

\item{Rf}{Risikofri rente.}

\item{Sp}{Porteføljens standardafvigelse.}
}
\value{
Sharpe Ratio (numerisk).
}
\description{
(Rp - Rf) / Sp
}
