#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom dplyr arrange bind_rows case_when coalesce collect dense_rank desc
#'     distinct everything filter group_by inner_join left_join mutate select summarise
#'     tbl tibble ungroup pull slice_head
#' @importFrom forcats fct_lump_n
#' @importFrom glue glue
#' @importFrom highcharter hcaes hchart hc_title hc_xAxis
#'     hc_yAxis JS renderHighchart hc_legend
#' @importFrom lubridate day year
#' @importFrom rio export
#' @importFrom rlang sym
#' @importFrom shinyhelper observe_helpers
#' @importFrom shinyjs hide show
#' @importFrom tidyr pivot_longer
#' @importFrom waiter Waitress
#' @noRd
app_server <- function(input, output, session) {
  # Countries module ----
  mod_countries_server("countries")

  # Cite module ----
  mod_cite_server("cite")

  # Hide boxes until viz is ready ----

  ## observe the button being pressed
  observeEvent(input$go, {
    if (input$go > 0) {
      show(id = "aggregated_trade")
      show(id = "detailed_trade")
    } else {
      hide(id = "aggregated_trade")
      hide(id = "detailed_trade")
    }
  })

  # Footer ----

  output$site_footer <- renderText({
    glue("<center><i>Open Trade Statistics {year(Sys.Date())}.</i></center>")
  })

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    rvtl(input)
    setBookmarkExclude(c(
      "shinyhelper-modal_params", "own", "sidebarCollapsed", "sidebarItemExpanded"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
