#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "welcome-container",
      tags$div(
        class = "text-center",
        tags$img(
          src = "https://shiny.tradestatistics.io/images/ots_logo_shiny.svg",
          alt = "Open Trade Statistics",
          class = "img-fluid mb-4 d-block mx-auto",
          style = "max-height:140px;"
        )
      ),
      tags$br(),
      tags$br(),
      tags$p("Open Trade Statistics started as a visualization project back in 2017."),
      tags$p(HTML('The information displayed here is based on <a href="https://comtradeplus.un.org/">UN Comtrade Plus</a> datasets. These figures do not include services or foreign direct investment.')),
      tags$p("Explore the country and product profiles using the menu on the left."),
      tags$br(),
      tags$p("Check the R package to download the product-level data displayed here:"),
      tags$a(
        href = "https://github.com/ropensci/tradestatistics",
        target = "_blank",
        class = "btn btn-primary",
        "R package"
      ),
      tags$br(),
      tags$br(),
      tags$p("If this resource is useful to you, please consider donating. The dashboard and the SQL database/API behind it will remain open and free of charge but there is a hosting cost."),
      tags$p(
        tags$a(
          href = "https://www.buymeacoffee.com/pacha", target = "_blank",
          tags$img(
            src = "https://raw.githubusercontent.com/pachadotdev/buymeacoffee-badges/main/bmc-blue.svg",
            alt = "Buy me a coffee",
            style = "height:34px;"
          )
        )
      )
    )
  )
}
