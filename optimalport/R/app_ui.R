#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      title = "Optimal Portefølje",
      id = "navbar",

      # Hjem-fane
      tabPanel(
        title = "Hjem",
        icon = icon("home"),
        fluidPage(
          titlePanel("Konstruktion af en Optimal Portefølje"),
          p("Denne applikation beregner en optimal portefølje baseret på Max Sharpe Ratio.
            Du kan vælge en startdato for dine beregninger, og se hvordan afkast og risiko påvirker allokeringen."),
          dateInput("start_date", "Vælg startdato for data:",
                    value = Sys.Date() - 365, format = "yyyy-mm-dd"),
          p("Startdatoen bestemmer, hvor langt tilbage vi kigger for at beregne afkast og risiko.
            En længere periode kan give et mere robust estimat, men inkluderer også ældre markedsdata.")
        )
      ),

      # Resultat-fane
      tabPanel(
        title = "Resultater",
        icon = icon("chart-line"),
        fluidPage(
          titlePanel("Porteføljeoptimering: Max Sharpe Ratio"),
          tableOutput("portfolio_table"),
          h3("Sharpe Ratio Sammenligning"),
          tableOutput("sharpe_table")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "optimalport"
    )
  )
}
