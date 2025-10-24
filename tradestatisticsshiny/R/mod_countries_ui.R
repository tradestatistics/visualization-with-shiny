#' @title Country profile UI-side function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
mod_countries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useWaiter(),
    fluidRow(
      # Filter -----
      col_12(
        h2("Filter")
      ),
      col_3(
        sliderInput(
          ns("y"),
          "Years",
          min = available_yrs_min(),
          max = available_yrs_max(),
          value = c(2018, 2023),
          sep = "",
          step = 1,
          ticks = FALSE,
          width = "100%"
        )
      ),
      col_3(
        selectInput(
          ns("r"),
          "Reporter",
          choices = sort(tradestatisticsshiny::reporters_display[
            tradestatisticsshiny::reporters_display != "ALL"
          ]),
          selected = "GBR",
          selectize = TRUE,
          width = "100%"
        )
      ),
      col_3(
        selectInput(
          ns("p"),
          "Partner",
          choices = c(
            "All countries" = "ALL",
            sort(tradestatisticsshiny::reporters_display[tradestatisticsshiny::reporters_display != "ALL"])
          ),
          selected = "ALL",
          selectize = TRUE,
          width = "100%"
        )
      ),
      col_3(
        selectInput(
          ns("d"),
          "Convert dollars to a fixed year",
          choices = c("No", available_yrs_deflator()),
          selected = "",
          selectize = TRUE,
          width = "100%"
        ) %>%
          helper(
            type = "inline",
            title = "Convert to dollars of the year",
            content = c("Uses present value and/or future value equations to adjust money value
                      by yearly changes in GDP deflator. The source for the GDP deflator data is The World Bank."),
            buttonLabel = "Got it!",
            easyClose = FALSE,
            fade = TRUE,
            size = "s"
          )
      ),
      col_12(
        align = "center",
        br(),
        actionButton(ns("go"), "Give me the country profile",
          class = "btn-primary"
        ),
        br(),
        br()
      ),

      # Trade ----

      hidden(
        div(
          id = ns("title_section"),
          fluidRow(
            col_12(
              htmlOutput(ns("title"), container = tags$h1)
            )
          )
        )
      ),

      ## Aggregated trade -----

      hidden(
        div(
          id = ns("aggregated_trade"),
          br(),
          br(),
          fluidRow(
            class = "row-equal-height",
            col_12(
              tabler_card(
                htmlOutput(ns("trd_stl"), container = tags$h2)
              )
            ),
            col_4(
              id = "aggregated_trade_left",
              tabler_card(
                htmlOutput(ns("trd_stl_exp"), container = tags$h4),
                htmlOutput(ns("trd_smr_exp"), container = tags$p),
                htmlOutput(ns("trd_stl_imp"), container = tags$h4),
                htmlOutput(ns("trd_smr_imp"), container = tags$p)
              )
            ),
            col_8(
              id = "aggregated_trade_right",
              tabler_card(
                d3po_output(ns("trd_exc_columns_agg"), height = "500px")
              )
            )
          )
        )
      ),

      ## Detailed trade ----

      hidden(
        div(
          id = ns("detailed_trade_exp"),
          br(),
          br(),
          fluidRow(
            class = "row-equal-height",
            ## Exports ----
            col_12(
              tabler_card(
                htmlOutput(ns("exp_tt_yr"), container = tags$h2)
              )
            ),
            col_6(
              id = "detailed_trade_col_exp_usd_left",
              tabler_card(
                d3po_output(ns("exp_col_min_yr_usd"), height = "400px")
              )
            ),
            col_6(
              id = "detailed_trade_col_exp_usd_right",
              tabler_card(
                d3po_output(ns("exp_col_max_yr_usd"), height = "400px")
              )
            ),
            col_6(
              id = "detailed_trade_tm_exp_left",
              tabler_card(
                d3po_output(ns("exp_tm_dtl_min_yr"), height = "500px")
              )
            ),
            col_6(
              id = "detailed_trade_tm_exp_right",
              tabler_card(
                d3po_output(ns("exp_tm_dtl_max_yr"), height = "500px")
              )
            )
          )
        ),
        hidden(
          div(
            id = ns("detailed_trade_imp"),
            br(),
            br(),
            fluidRow(
              ## Imports ----
              col_12(
                tabler_card(
                  htmlOutput(ns("imp_tt_yr"), container = tags$h2)
                )
              ),
              col_6(
                id = "detailed_trade_col_imp_usd_left",
                tabler_card(
                  d3po_output(ns("imp_col_min_yr_usd"), height = "400px")
                )
              ),
              col_6(
                id = "detailed_trade_col_imp_usd_right",
                tabler_card(
                  d3po_output(ns("imp_col_max_yr_usd"), height = "400px")
                )
              ),
              col_6(
                id = "detailed_trade_tm_imp_left",
                tabler_card(
                  d3po_output(ns("imp_tm_dtl_min_yr"), height = "500px")
                )
              ),
              col_6(
                id = "detailed_trade_tm_imp_right",
                tabler_card(
                  d3po_output(ns("imp_tm_dtl_max_yr"), height = "500px")
                )
              )
            )
          )
        ),

        ## Download ----
        hidden(
          div(
            id = ns("download_data"),
            br(),
            br(),
            fluidRow(
              col_12(tabler_card(
                htmlOutput(ns("dwn_stl"), container = tags$h2),
                htmlOutput(ns("dwn_txt"), container = tags$p),
                uiOutput(ns("dwn_fmt")),
                br(),
                uiOutput(ns("dwn_agg")),
                br(),
                uiOutput(ns("dwn_dtl"))
              ))
            )
          )
        )
      )
    )
  )
}
