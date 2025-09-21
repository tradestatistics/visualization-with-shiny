#' products UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_products_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' products Server Functions
#'
#' @noRd
mod_products_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_products_ui("products_1")

## To be copied in the server
# mod_products_server("products_1")
