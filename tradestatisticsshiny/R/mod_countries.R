#' countries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow selectInput sliderInput actionButton
#'     htmlOutput uiOutput h2 tags div
#' @importFrom highcharter highchartOutput
#' @importFrom shinyhelper helper
mod_countries_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
          choices = sort(available_reporters_iso()[
            available_reporters_iso() != "ALL"
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
            sort(available_reporters_iso()[available_reporters_iso() != "ALL"])
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
        actionButton(ns("go"), "Give me the country profile",
          class = "btn-primary"
        )
      ),

      # Trade ----

      col_12(
        htmlOutput(ns("title"), container = tags$h2)
      ),

      ## Aggregated trade -----

      div(
        id = ns("aggregated_trade"),
        col_12(
          htmlOutput(ns("trd_stl"), container = tags$h3)
        ),
        col_3(
          htmlOutput(ns("trd_stl_exp"), container = tags$h4),
          htmlOutput(ns("trd_smr_exp"), container = tags$p),
          htmlOutput(ns("trd_stl_imp"), container = tags$h4),
          htmlOutput(ns("trd_smr_imp"), container = tags$p)
        ),
        col_9(
          highchartOutput(ns("trd_exc_columns_agg"), height = "500px")
        )
      ),

      ## Detailed trade ----

      div(
        id = ns("detailed_trade"),
        col_12(
          htmlOutput(ns("exp_tt_yr"), container = tags$h3)
        ),
        col_6(
          htmlOutput(ns("exp_col_min_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_min_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_max_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_max_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_min_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_min_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_max_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_max_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_tt_min_yr"), container = tags$h4),
          highchartOutput(ns("exp_tm_dtl_min_yr"), height = "500px")
        ),
        col_6(
          htmlOutput(ns("exp_tt_max_yr"), container = tags$h4),
          highchartOutput(ns("exp_tm_dtl_max_yr"), height = "500px")
        ),
        col_12(
          htmlOutput(ns("imp_tt_yr"), container = tags$h3)
        ),
        col_6(
          htmlOutput(ns("imp_col_min_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_min_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_max_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_max_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_min_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_min_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_max_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_max_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_tt_min_yr"), container = tags$h4),
          highchartOutput(ns("imp_tm_dtl_min_yr"), height = "500px")
        ),
        col_6(
          htmlOutput(ns("imp_tt_max_yr"), container = tags$h4),
          highchartOutput(ns("imp_tm_dtl_max_yr"), height = "500px")
        ),
        col_12(
          htmlOutput(ns("dwn_stl"), container = tags$h3),
          htmlOutput(ns("dwn_txt"), container = tags$p),
          uiOutput(ns("dwn_fmt")),
          uiOutput(ns("dwn_agg")),
          uiOutput(ns("dwn_dtl"))
        )
      )
    )
  )
}

#' countries Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive eventReactive observe observeEvent
#'     renderText renderUI updateSelectizeInput downloadHandler req
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
mod_countries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Connect to SQL ----
    con <- sql_con()
    
    session$onSessionEnded(function() {
      if (!is.null(con) && dbIsValid(con)) {
        poolClose(con)
      }
    })

    # User inputs ----
    observe_helpers()

    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      return(y)
    })

    inp_r <- reactive({
      input$r
    }) # reporter

    inp_p <- reactive({
      input$p
    }) # partner

    inp_d <- reactive({
      input$d
    }) # adjust dollar

    inp_fmt <- reactive({
      input$fmt
    }) # format

    tbl_agg <- "yrp"

    tbl_dtl <- "yrpc"

    # Human-readable reporter/partner names for glue templates. Fallback to
    # the code when no display name is available.
    rname <- eventReactive(input$go, {
      out <- names(available_reporters_iso()[available_reporters_iso() == inp_r()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_r())
      }
      out
    })

    pname <- eventReactive(input$go, {
      out <- names(available_reporters_iso()[available_reporters_iso() == inp_p()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_p())
      }
      out
    })

    # Ensure data frame contains expected columns to avoid mutate/group_by errors
    ensure_cols <- function(df, cols) {
      for (c in cols) {
        if (!c %in% names(df)) df[[c]] <- NA_character_
      }
      df
    }

    title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() } multilateral trade between { min(inp_y()) } and { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { p_add_the(pname()) } { pname() } trade between { min(inp_y()) } and { max(inp_y()) }")
      }
    })

    # Visualize ----

    ## Data ----

    wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

    df_agg <- reactive({
      wt$notify(position = "br")

      d <- tbl(con, tbl_agg)

      if (inp_p() == "ALL") {
        d <- d %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r()
          ) %>%
          group_by(!!sym("year"), !!sym("reporter_iso")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        d <- d %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r() &
              !!sym("partner_iso") == !!inp_p()
          )
      }

      d <- d %>% collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      wt$inc(2.5)

      return(d)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    df_dtl <- reactive({
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        d <- d %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r()
          )
      } else {
        d <- d %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r() &
              !!sym("partner_iso") == !!inp_p()
          )
      }

      d <- d %>%
        inner_join(
          tbl(con, "commodities") %>%
            distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color"), !!sym("commodity_code_short"), !!sym("commodity_name")),
          by = c("commodity_code", "section_code")
        )

      d <- collect(d)

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      wt$inc(2.5)

      return(d)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    tr_tbl_agg <- eventReactive(input$go, {
      df_agg() %>%
        select(!!sym("year"), !!sym("trade_value_usd_exp"), !!sym("trade_value_usd_imp"))
    })

    exp_val_min_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        select(!!sym("trade_value_usd_exp")) %>%
        as.numeric()
    })

    exp_val_max_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        select(!!sym("trade_value_usd_exp")) %>%
        as.numeric()
    })

    imp_val_min_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        select(!!sym("trade_value_usd_imp")) %>%
        as.numeric()
    })

    imp_val_max_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        select(!!sym("trade_value_usd_imp")) %>%
        as.numeric()
    })

    exp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_min_yr())
    })

    exp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_max_yr())
    })

    imp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_min_yr())
    })

    imp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_max_yr())
    })

    exports_growth <- eventReactive(input$go, {
      growth_rate(
        exp_val_max_yr(), exp_val_min_yr(), inp_y()
      )
    })

    exports_growth_2 <- eventReactive(input$go, {
      show_percentage(exports_growth())
    })

    exports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increased", "decreased")
    })

    exports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increase", "decrease")
    })

    imports_growth <- eventReactive(input$go, {
      growth_rate(
        imp_val_max_yr(), imp_val_min_yr(), inp_y()
      )
    })

    imports_growth_2 <- eventReactive(input$go, {
      show_percentage(imports_growth())
    })

    imports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })

    imports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })

    trd_rankings <- eventReactive(input$go, {
      min_max_y <- c(min(inp_y()), max(inp_y()))

      # Always get ALL partners for consistent ranking calculation
      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") %in% min_max_y &
            !!sym("reporter_iso") == !!inp_r()
        ) %>%
        collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      d <- d %>%
        # Keep all partners including unspecified for accurate share calculation
        mutate(
          trd_value_usd_bal = !!sym("trade_value_usd_exp") + !!sym("trade_value_usd_imp")
        ) %>%
        group_by(!!sym("year")) %>%
        mutate(
          bal_rank = dense_rank(desc(!!sym("trd_value_usd_bal"))),
          exp_share = !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
          imp_share = !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp"), na.rm = TRUE)
        ) %>%
        ungroup()

      return(d)
    })

    # Helper function to get ranking with tie information
    get_ranking_with_ties <- function(year_val) {
      if (inp_p() == "ALL") {
        return("N/A")  # No ranking for multilateral trade
      }
      
      rankings_data <- trd_rankings() %>%
        filter(!!sym("year") == year_val, !!sym("reporter_iso") == !!inp_r())
      
      partner_rank <- rankings_data %>%
        filter(!!sym("partner_iso") == !!inp_p()) %>%
        pull(!!sym("bal_rank"))
      
      if (length(partner_rank) == 0 || is.na(partner_rank)) {
        return("N/A")
      }
      
      # Check for ties
      tied_partners <- rankings_data %>%
        filter(!!sym("bal_rank") == partner_rank, !!sym("partner_iso") != !!inp_p()) %>%
        nrow()
      
      if (tied_partners > 0) {
        return(paste0(partner_rank, " (tied with ", tied_partners, " other", 
                     ifelse(tied_partners == 1, "", "s"), ")"))
      } else {
        return(as.character(partner_rank))
      }
    }

    trd_rankings_no_min_yr <- eventReactive(input$go, {
      get_ranking_with_ties(min(inp_y()))
    })

    trd_rankings_no_max_yr <- eventReactive(input$go, {
      get_ranking_with_ties(max(inp_y()))
    })

    trd_rankings_remained <- eventReactive(input$go, {
      min_rank <- trd_rankings_no_min_yr()
      max_rank <- trd_rankings_no_max_yr()
      
      if (min_rank == "N/A" || max_rank == "N/A") {
        return("was")
      }
      
      # Extract just the numeric part for comparison (remove tie information)
      min_rank_num <- as.numeric(gsub(" \\(.*\\)", "", min_rank))
      max_rank_num <- as.numeric(gsub(" \\(.*\\)", "", max_rank))
      
      ifelse(
        min_rank_num == max_rank_num,
        "remained",
        "moved to"
      )
    })

    trd_rankings_exp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == min(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("exp_share")) %>%
        as.numeric()
      
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
      return(result)
    })

    trd_rankings_exp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_exp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == max(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("exp_share")) %>%
        as.numeric()
      
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
      return(result)
    })

    trd_rankings_exp_share_max_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == min(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("imp_share")) %>%
        as.numeric()
      
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
      return(result)
    })

    trd_rankings_imp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_imp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == max(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("imp_share")) %>%
        as.numeric()
      
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
      return(result)
    })

    trd_rankings_imp_share_max_yr_2 <- eventReactive(input$go, {
      wt$inc(1)

      share_val <- trd_rankings_imp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    ### Text/Visual elements ----

    trd_smr_txt_exp <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("The exports of { r_add_the(rname()) } { rname() } to the World { exports_growth_increase_decrease() } from
                            { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) }
                            (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() }).")
      } else {
        glue("The exports of { r_add_the(rname()) } { rname() } to { p_add_the(pname()) } { pname() } { exports_growth_increase_decrease() } from
                            { exp_val_min_yr_2() } in { min(inp_y()) }
                            to { exp_val_max_yr_2() } in { max(inp_y()) } (annualized { exports_growth_increase_decrease_2() } of
                            { exports_growth_2() }). { p_add_the(pname()) } { pname() } was the No. { trd_rankings_no_min_yr() } trading partner of
            { r_add_the(rname()) } { rname() } in { min(inp_y()) } (represented { trd_rankings_exp_share_min_yr_2() } of its exports), and
                            then { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } (represented { trd_rankings_exp_share_max_yr_2() }
                            of its exports).")
      }
    })

    trd_smr_txt_imp <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("The imports of { r_add_the(rname()) } { rname() } from the World { imports_growth_increase_decrease() } from
                           { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) }
                           (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() }).")
      } else {
        glue("The imports of { r_add_the(rname()) } { rname() } from { p_add_the(pname()) } { pname() } { imports_growth_increase_decrease() } from
                            { imp_val_min_yr_2() } in { min(inp_y()) }
                            to { imp_val_max_yr_2() } in { max(inp_y()) } (annualized { imports_growth_increase_decrease_2() } of
                            { imports_growth_2() }). { p_add_the(pname()) } { pname() } was the No. { trd_rankings_no_min_yr() } trading partner of
            { r_add_the(rname()) } { rname() } in { min(inp_y()) } (represented { trd_rankings_imp_share_min_yr_2() } of its imports), and
                            then { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } (represented { trd_rankings_imp_share_max_yr_2() }
                            of its imports).")
      }
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() } multilateral trade between { min(inp_y()) } and { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { p_add_the(pname()) } { pname() } exchange between { min(inp_y()) } and { max(inp_y()) }")
      }
    })

    trd_exc_columns_agg <- reactive({
      d <- tr_tbl_agg()

      d <- tibble(
        year = d$year,
        trade = d$trade_value_usd_exp,
        flow = "Exports"
      ) %>%
        bind_rows(
          tibble(
            year = d$year,
            trade = d$trade_value_usd_imp,
            flow = "Imports"
          )
        ) %>%
        mutate(year = as.character(year))

      # Create named color vector for proper mapping
      flow_colors <- setNames(
        c("#67c090", "#26667f"),
        c("Exports", "Imports")
      )

      wt$inc(1)

      hchart(d,
        "column",
        hcaes(x = "year", y = "trade", group = "flow"),
        color = flow_colors,
        tooltip = list(
          pointFormatter = custom_tooltip_short()
        )
      ) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(
            formatter = JS("function() { return this.value / 1000000000 }")
          )
        ) %>%
        hc_title(text = trd_exc_columns_title())
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Exports of { r_add_the(rname()) } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Exports of { r_add_the(rname()) } { rname() } to { p_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    # Export column chart titles
    exp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) } (USD)")
    })

    exp_col_min_yr_pct_tt <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) } (%)")
    })

    exp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) } (USD)")
    })

    exp_col_max_yr_pct_tt <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) } (%)")
    })

    # Export column charts
    exp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_exp"),
               color = "#85cca6",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = exp_col_min_yr_usd_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "trade_value_usd_exp"),
               color = "#85cca6",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = exp_col_min_yr_usd_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_min_yr_pct <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")), 1)) %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
               color = "#85cca6",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = exp_col_min_yr_pct_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")), 1)) %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "percentage"),
               color = "#85cca6",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = exp_col_min_yr_pct_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_exp"),
               color = "#67c090",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = exp_col_max_yr_usd_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "trade_value_usd_exp"),
               color = "#67c090",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = exp_col_max_yr_usd_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_pct <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")), 1)) %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
               color = "#67c090",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = exp_col_max_yr_pct_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_exp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")), 1)) %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "percentage"),
               color = "#67c090",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = exp_col_max_yr_pct_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    # top_sections: return a data.frame with 9 largest sections + 'other' and colors
    top_sections <- reactive({
      req(inp_y())

      # compute exchange (exports + imports) by section across selected years
      totals <- df_dtl() %>%
        filter(year %in% !!inp_y()) %>%
        group_by(section_code, section_name, section_color) %>%
        summarise(exchange = sum(trade_value_usd_exp + trade_value_usd_imp, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(exchange))

      # top 9 codes by exchange
      top9 <- totals %>% slice_head(n = 9)

      out <- top9 %>% select(section_code, section_name, section_color)
      # append canonical 'Other products' at the end
      out <- bind_rows(out, tibble(section_code = "other", section_name = "Other products", section_color = "#434348"))
      out
    })

    # section totals for the most recent year
    section_totals_recent <- reactive({
      req(inp_y())

      yr <- max(inp_y())

      d <- df_dtl() %>%
        filter(year == !!yr) %>%
        group_by(section_code) %>%
        summarise(
          trade_exp = sum(trade_value_usd_exp, na.rm = TRUE),
          trade_imp = sum(trade_value_usd_imp, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(exchange = trade_exp + trade_imp)

      # join canonical mapping (if available) and provide defaults
      cs <- tryCatch(top_sections(), error = function(e) NULL)
      if (!is.null(cs)) {
        d <- d %>%
          left_join(cs, by = "section_code") %>%
          mutate(
            section_name = coalesce(section_name, "Other products"),
            section_color = coalesce(section_color, "#434348")
          ) %>%
          select(section_code, section_name, section_color, trade_exp, trade_imp, exchange)
      }

      d
    })

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_exp", con = con)

      d2 <- p_colors(d, con = con)

      p_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_exp", con = con)

      d2 <- p_colors(d, con = con)

      p_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Imports of { r_add_the(rname()) } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Imports of { r_add_the(rname()) } { rname() } from { p_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) }")
    })

    # Import column chart titles
    imp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) } (USD)")
    })

    imp_col_min_yr_pct_tt <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) } (%)")
    })

    imp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) } (USD)")
    })

    imp_col_max_yr_pct_tt <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) } (%)")
    })

    # Import column charts
    imp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
               color = "#518498",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = imp_col_min_yr_usd_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "trade_value_usd_imp"),
               color = "#518498",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = imp_col_min_yr_usd_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_min_yr_pct <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
               color = "#518498",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = imp_col_min_yr_pct_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "percentage"),
               color = "#518498",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = imp_col_min_yr_pct_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
               color = "#26667f",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = imp_col_max_yr_usd_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "trade_value_usd_imp"),
               color = "#26667f",
               tooltip = list(pointFormat = "<b>{point.y:,.0f} USD</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "USD billion"),
                   labels = list(formatter = JS("function() { return this.value / 1000000000 }"))) %>%
          hc_title(text = imp_col_max_yr_usd_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_pct <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(country_name = fct_lump_n(f = !!sym("country_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Rest of the world")) %>%
          group_by(!!sym("country_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
          mutate(country_name = factor(!!sym("country_name"), 
                                       levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")))

        hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
               color = "#26667f",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Country")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = imp_col_max_yr_pct_tt())
      } else {
        # Show top 4 sections + "Other products" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!inp_r() &
            !!sym("partner_iso") == !!inp_p()
          )

        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color")),
            by = c("commodity_code", "section_code")
          ) %>%
          collect()

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          mutate(section_name = fct_lump_n(f = !!sym("section_name"), 
                                           n = 4, 
                                           w = !!sym("trade_value_usd_imp"),
                                           other_level = "Other products")) %>%
          group_by(!!sym("section_name")) %>%
          summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
          mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
          mutate(section_name = factor(!!sym("section_name"), 
                                       levels = c(setdiff(unique(!!sym("section_name")), "Other products"), "Other products")))

        hchart(d, "column", hcaes(x = "section_name", y = "percentage"),
               color = "#26667f",
               tooltip = list(pointFormat = "<b>{point.y}%</b>")) %>%
          hc_xAxis(title = list(text = "Product Section")) %>%
          hc_yAxis(title = list(text = "Percentage (%)")) %>%
          hc_title(text = imp_col_max_yr_pct_tt())
      }
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_tm_dtl_min_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_imp", con = con)

      d2 <- p_colors(d, con = con)

      p_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_imp", con = con)

      d2 <- p_colors(d, con = con)

      wt$inc(3)

      out <- p_to_highcharts(d, d2)

      wt$close()
      return(out)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Dynamic / server side selectors ----

    observeEvent(input$r, {
      updateSelectizeInput(session, "p",
        choices = sort(available_reporters_iso()[
          available_reporters_iso() != input$r
        ]),
        selected = "ALL",
        server = TRUE
      )
    })

    ## Download ----

    dwn_stl <- eventReactive(input$go, {
      "Download country data"
    })

    dwn_txt <- eventReactive(input$go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
    })

    dwn_fmt <- eventReactive(input$go, {
      selectInput(
        ns("fmt"),
        "Format",
        choices = available_formats(),
        selected = NULL,
        selectize = TRUE
      )
    })

    ## Outputs ----

    ## Titles ----

    output$title <- renderText({
      title()
    })

    ## Country profile ----

    ### Trade ----

    output$trd_stl <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Total multilateral Exports and Imports")
      } else {
        glue("Total bilateral Exports and Imports")
      }
    })

    output$trd_stl_exp <- eventReactive(input$go, {
      "Exports"
    })
    output$trd_stl_imp <- eventReactive(input$go, {
      "Imports"
    })

    output$trd_smr_exp <- renderText(trd_smr_txt_exp())
    output$trd_smr_imp <- renderText(trd_smr_txt_imp())

    output$trd_exc_columns_agg <- renderHighchart({
      trd_exc_columns_agg()
    })

    ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())
    
    # Export column chart outputs
    output$exp_col_min_yr_usd_tt <- renderText(exp_col_min_yr_usd_tt())
    output$exp_col_min_yr_usd <- renderHighchart({
      exp_col_min_yr_usd()
    })
    output$exp_col_min_yr_pct_tt <- renderText(exp_col_min_yr_pct_tt())
    output$exp_col_min_yr_pct <- renderHighchart({
      exp_col_min_yr_pct()
    })
    output$exp_col_max_yr_usd_tt <- renderText(exp_col_max_yr_usd_tt())
    output$exp_col_max_yr_usd <- renderHighchart({
      exp_col_max_yr_usd()
    })
    output$exp_col_max_yr_pct_tt <- renderText(exp_col_max_yr_pct_tt())
    output$exp_col_max_yr_pct <- renderHighchart({
      exp_col_max_yr_pct()
    })
    
    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- renderHighchart({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- renderHighchart({
      exp_tm_dtl_max_yr()
    })

    ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())
    
    # Import column chart outputs
    output$imp_col_min_yr_usd_tt <- renderText(imp_col_min_yr_usd_tt())
    output$imp_col_min_yr_usd <- renderHighchart({
      imp_col_min_yr_usd()
    })
    output$imp_col_min_yr_pct_tt <- renderText(imp_col_min_yr_pct_tt())
    output$imp_col_min_yr_pct <- renderHighchart({
      imp_col_min_yr_pct()
    })
    output$imp_col_max_yr_usd_tt <- renderText(imp_col_max_yr_usd_tt())
    output$imp_col_max_yr_usd <- renderHighchart({
      imp_col_max_yr_usd()
    })
    output$imp_col_max_yr_pct_tt <- renderText(imp_col_max_yr_pct_tt())
    output$imp_col_max_yr_pct <- renderHighchart({
      imp_col_max_yr_pct()
    })
    
    output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
    output$imp_tm_dtl_min_yr <- renderHighchart({
      imp_tm_dtl_min_yr()
    })
    output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
    output$imp_tm_dtl_max_yr <- renderHighchart({
      imp_tm_dtl_max_yr()
    })

    ## Download ----

    output$dwn_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_dtl(), filename)
      }
    )

    output$dwn_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_agg(), filename)
      }
    )

    output$dwn_stl <- renderText({
      dwn_stl()
    })
    output$dwn_txt <- renderText({
      dwn_txt()
    })
    output$dwn_fmt <- renderUI({
      dwn_fmt()
    })

    output$dwn_dtl <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_dtl_pre"), label = "Detailed data")
    })

    output$dwn_agg <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_agg_pre"), label = "Aggregated data")
    })

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
  })
}
