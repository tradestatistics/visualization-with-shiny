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
  # Connect to SQL ----

  con <- sql_con()
  onStop(function() {
    pool::poolClose(con)
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

  tbl_agg <- eventReactive(input$go, {
    ifelse(inp_p() == "ALL", "yr", "yrp")
  })

  tbl_dtl <- eventReactive(input$go, {
    ifelse(inp_p() == "ALL", "yrc", "yrpc")
  })

  # Human-readable reporter/partner names for glue templates. Fallback to
  # the code when no display name is available.
  rname <- eventReactive(input$go, {
    out <- names(available_reporters_iso()[available_reporters_iso() == inp_r()])
    if (length(out) == 0 || is.na(out) || nchar(out) == 0) return(inp_r())
    out
  })

  # diagnostic: section totals for the most recent year
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

  pname <- eventReactive(input$go, {
    out <- names(available_reporters_iso()[available_reporters_iso() == inp_p()])
    if (length(out) == 0 || is.na(out) || nchar(out) == 0) return(inp_p())
    out
  })

  # use p_add_the function from common package with inp_p() when needed

  # Small name helpers (restore behaviour when earlier definitions were lost)
  # rname/pname not needed; use inp_r()/inp_p() directly in glue expressions

  # Ensure data frame contains expected columns to avoid mutate/group_by errors
  ensure_cols <- function(df, cols) {
    for (c in cols) {
      if (!c %in% names(df)) df[[c]] <- NA_character_
    }
    df
  }

  title <- eventReactive(input$go, {
    switch(tbl_dtl(),
  "yrc" = glue("{ r_add_upp_the(rname()) } { rname() } multilateral trade between { min(inp_y()) } and { max(inp_y()) }"),
  "yrpc" = glue("{ r_add_upp_the(rname()) } { rname() } and { p_add_the(pname()) } { pname() } trade between { min(inp_y()) } and { max(inp_y()) }")
    )
  })

  # Visualize ----

  ## Data ----

  wt <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

  df_agg <- reactive({
    wt$notify(position = "br")

    d <- tbl(con, tbl_agg())

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
    d <- tbl(con, tbl_dtl())

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
    bindCache(inp_y(), inp_r(), inp_p(), inp_d(), tbl_dtl()) %>%
    bindEvent(input$go)


  # Build a full sections summary table (canonical colors + trade values) and
  # write to /tmp for local inspection. This keeps all sections and avoids any
  # 'other' lumping; useful for debugging treemap visibility.
  sections_summary <- reactive({
    # collect detailed data and canonical commodities
    d_dtl <- df_dtl() %>%
      filter(!!sym("year") %in% !!inp_y()) %>%
      collect()

    commodities_ref <- tbl(con, "commodities") %>%
      distinct(!!sym("section_code"), !!sym("section_name"), !!sym("section_color"), !!sym("commodity_code_short"), !!sym("commodity_code")) %>%
      collect()

    # ensure commodity identifiers exist in d_dtl; prefer commodity_code_short when present
    if (!"commodity_code_short" %in% names(d_dtl)) {
      # attempt to derive short code from commodity_code if possible
      d_dtl <- d_dtl %>% mutate(commodity_code_short = as.character(!!sym("commodity_code")))
    }
    if (!"commodity_name" %in% names(d_dtl)) {
      d_dtl <- d_dtl %>% mutate(commodity_name = NA_character_)
    }

    # aggregate raw data by year/section/commodity using available identifiers
    d_raw <- d_dtl %>%
      group_by(year, section_code, section_name, section_color, commodity_code_short, commodity_name) %>%
      summarise(
        trade_exp = sum(trade_value_usd_exp, na.rm = TRUE),
        trade_imp = sum(trade_value_usd_imp, na.rm = TRUE),
        .groups = "drop"
      )

    # left-join onto canonical reference using commodity_code_short when possible
    master <- commodities_ref %>%
      select(section_code, section_name, section_color, commodity_code_short, commodity_code) %>%
      distinct() %>%
      left_join(d_raw, by = c("section_code", "section_name", "commodity_code_short")) %>%
      mutate(trade_exp = coalesce(trade_exp, 0), trade_imp = coalesce(trade_imp, 0)) %>%
      arrange(section_name)

    master
  })

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
      # filter(!!sym("partner_iso") != "0-unspecified") %>%
      mutate(
        trd_value_usd_bal = !!sym("trade_value_usd_exp") + !!sym("trade_value_usd_imp")
      ) %>%
      group_by(!!sym("year")) %>%
      mutate(
        bal_rank = dense_rank(desc(!!sym("trd_value_usd_bal"))),
        exp_share = !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp")),
        imp_share = !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp"))
      ) %>%
      ungroup()

    return(d)
  })

  trd_rankings_no_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("bal_rank")) %>%
      as.character()
  })

  trd_rankings_no_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("bal_rank")) %>%
      as.character()
  })

  trd_rankings_remained <- eventReactive(input$go, {
    ifelse(
      trd_rankings_no_min_yr() == trd_rankings_no_max_yr(),
      "remained",
      "moved to"
    )
  })

  trd_rankings_exp_share_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("exp_share")) %>%
      as.numeric()
  })

  trd_rankings_exp_share_min_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_exp_share_min_yr())
  })

  trd_rankings_exp_share_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("exp_share")) %>%
      as.numeric()
  })

  trd_rankings_exp_share_max_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_exp_share_max_yr())
  })

  trd_rankings_imp_share_min_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == min(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("imp_share")) %>%
      as.numeric()
  })

  trd_rankings_imp_share_min_yr_2 <- eventReactive(input$go, {
    show_percentage(trd_rankings_imp_share_min_yr())
  })

  trd_rankings_imp_share_max_yr <- eventReactive(input$go, {
    trd_rankings() %>%
      ungroup() %>%
      filter(
        !!sym("year") == max(!!inp_y()),
        !!sym("reporter_iso") == !!inp_r(),
        !!sym("partner_iso") == !!inp_p()
      ) %>%
      select(!!sym("imp_share")) %>%
      as.numeric()
  })

  trd_rankings_imp_share_max_yr_2 <- eventReactive(input$go, {
    wt$inc(1)

    show_percentage(trd_rankings_imp_share_max_yr())
  })

  ### Text/Visual elements ----

  trd_smr_txt_exp <- eventReactive(input$go, {
    switch(tbl_agg(),
  "yr" = glue("The exports of { r_add_the(rname()) } { rname() } to the World { exports_growth_increase_decrease() } from
                          { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) }
                          (annualized { exports_growth_increase_decrease_2() } of { exports_growth_2() })."),
  "yrp" = glue("The exports of { r_add_the(rname()) } { rname() } to { p_add_the(pname()) } { pname() } { exports_growth_increase_decrease() } from
                          { exp_val_min_yr_2() } in { min(inp_y()) }
                          to { exp_val_max_yr_2() } in { max(inp_y()) } (annualized { exports_growth_increase_decrease_2() } of
                          { exports_growth_2() }). { p_add_the(pname()) } { pname() } was the No. { trd_rankings_no_min_yr() } trading partner of
          { r_add_the(rname()) } { rname() } in { min(inp_y()) } (represented { trd_rankings_exp_share_min_yr_2() } of its exports), and
                          then { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } (represented { trd_rankings_exp_share_max_yr_2() }
                          of its exports).")
    )
  })

  trd_smr_txt_imp <- eventReactive(input$go, {
    switch(tbl_agg(),
  "yr" = glue("The imports of { r_add_the(rname()) } { rname() } to the World { imports_growth_increase_decrease() } from
                         { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) }
                         (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() })."),
  "yrp" = glue("The imports of { r_add_the(rname()) } { rname() } to { p_add_the(pname()) } { pname() } { imports_growth_increase_decrease() } from
                          { imp_val_min_yr_2() } in { min(inp_y()) }
                          to { imp_val_max_yr_2() } in { max(inp_y()) } (annualized { imports_growth_increase_decrease_2() } of
                          { imports_growth_2() }). { p_add_the(pname()) } { pname() } was the No. { trd_rankings_no_min_yr() } trading partner of
          { r_add_the(rname()) } { rname() } in { min(inp_y()) } (represented { trd_rankings_imp_share_min_yr_2() } of its imports), and
                          then { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } (represented { trd_rankings_imp_share_max_yr_2() }
                          of its imports).")
    )
  })

  trd_exc_columns_title <- eventReactive(input$go, {
    switch(tbl_agg(),
  "yr" = glue("{ r_add_upp_the(rname()) } { rname() } multilateral trade between { min(inp_y()) } and { max(inp_y()) }"),
  "yrp" = glue("{ r_add_upp_the(rname()) } { rname() } and { p_add_the(pname()) } { pname() } exchange between { min(inp_y()) } and { max(inp_y()) }")
    )
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

    wt$inc(1)

    hchart(d,
      "column",
      hcaes(x = "year", y = "trade", group = "flow"),
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
    switch(tbl_dtl(),
  "yrc" = glue("Exports of { r_add_the(rname()) } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product"),
  "yrpc" = glue("Exports of { r_add_the(rname()) } { rname() } to { p_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
    )
  })


  exp_col_dtl_yr <- reactive({
    # Build columns using the canonical top-9 sections + 'Other products'
    cs <- top_sections()

    # aggregate section totals for min and max years (one row per section per year)
    years <- c(min(inp_y()), max(inp_y()))
    
    d <- df_dtl() %>%
      filter(year %in% !!years) %>%
      group_by(year, section_code, section_name, section_color) %>%
      summarise(
        trade_exp = sum(trade_value_usd_exp, na.rm = TRUE),
        trade_imp = sum(trade_value_usd_imp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        # exports chart -> use exports totals
        trade = trade_exp,
        exchange = trade_exp + trade_imp
      ) %>%
      # collapse non-top sections into 'other'
      mutate(
        section_code_mapped = ifelse(section_code %in% cs$section_code, section_code, "other")
      ) %>%
      # join with canonical sections to get proper names/colors for "other"
      left_join(cs, by = c("section_code_mapped" = "section_code"), suffix = c("_orig", "_canonical")) %>%
      mutate(
        section_name_final = coalesce(section_name_canonical, section_name_orig, "Other products"),
        section_color_final = coalesce(section_color_canonical, section_color_orig, "#434348")
      ) %>%
      group_by(year, section_code_mapped, section_name_final, section_color_final) %>%
      summarise(
        trade = sum(trade, na.rm = TRUE),
        exchange = sum(exchange, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(year, section_code = section_code_mapped, section_name = section_name_final, section_color = section_color_final, trade, exchange) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, desc(exchange)) %>%
      # ensure plotting order: factor section_name with 'Other products' last
      mutate(section_name = factor(as.character(section_name), levels = c(setdiff(cs$section_name, "Other products"), "Other products")))

    hchart(d,
      "column",
      hcaes(
        x = "section_name", y = "trade", group = "year",
        color = "section_color"
      ),
      tooltip = list(
        pointFormatter = custom_tooltip_short()
      )
    ) %>%
      hc_xAxis(title = list(text = "Product")) %>%
      hc_yAxis(
        title = list(text = "USD billion"),
        labels = list(
          formatter = JS("function() { return this.value / 1000000000 }")
        )
      ) %>%
      hc_title(text = glue("Exports in { min(inp_y()) } and { max(inp_y()) }")) %>%
      hc_legend(enabled = FALSE)
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
    switch(tbl_dtl(),
  "yrc" = glue("Imports of { r_add_the(rname()) } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product"),
  "yrpc" = glue("Imports of { r_add_the(rname()) } { rname() } from { p_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
    )
  })

  imp_tt_min_yr <- eventReactive(input$go, {
    glue("Imports in { min(inp_y()) }")
  })


  imp_col_dtl_yr <- reactive({
    # Use top_sections (exchange-based) for canonical mapping
    cs <- top_sections()

    years <- c(min(inp_y()), max(inp_y()))

    d <- df_dtl() %>%
      filter(year %in% !!years) %>%
      group_by(year, section_code, section_name, section_color) %>%
      summarise(
        trade_imp = sum(trade_value_usd_imp, na.rm = TRUE),
        trade_exp = sum(trade_value_usd_exp, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        trade = trade_imp,
        trade_exchange = trade_imp + trade_exp
      ) %>%
      # collapse non-top sections into 'other'
      mutate(
        section_code_mapped = ifelse(section_code %in% cs$section_code, section_code, "other")
      ) %>%
      # join with canonical sections to get proper names/colors for "other"
      left_join(cs, by = c("section_code_mapped" = "section_code"), suffix = c("_orig", "_canonical")) %>%
      mutate(
        section_name_final = coalesce(section_name_canonical, section_name_orig, "Other products"),
        section_color_final = coalesce(section_color_canonical, section_color_orig, "#434348")
      ) %>%
      group_by(year, section_code_mapped, section_name_final, section_color_final) %>%
      summarise(
        trade = sum(trade, na.rm = TRUE),
        trade_exchange = sum(trade_exchange, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(year, section_code = section_code_mapped, section_name = section_name_final, section_color = section_color_final, trade, trade_exchange) %>%
      mutate(year = as.character(year)) %>%
      arrange(year, desc(trade_exchange)) %>%
      # ensure plotting order: factor section_name with 'Other products' last
      mutate(section_name = factor(as.character(section_name), levels = c(setdiff(cs$section_name, "Other products"), "Other products")))

    hchart(d,
      "column",
      hcaes(
        x = "section_name", y = "trade", group = "year",
        color = "section_color"
      ),
      tooltip = list(
        pointFormatter = custom_tooltip_short()
      )
    ) %>%
      hc_xAxis(title = list(text = "Product")) %>%
      hc_yAxis(
        title = list(text = "USD billion"),
        labels = list(
          formatter = JS("function() { return this.value / 1000000000 }")
        )
      ) %>%
      hc_title(text = glue("Imports in { min(inp_y()) } and { max(inp_y()) }")) %>%
      hc_legend(enabled = FALSE)
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

  # Outputs ----

  ## Titles ----

  output$title <- renderText({
    title()
  })

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

  ## Country profile ----

  ### Trade ----

  output$trd_stl <- eventReactive(input$go, {
    switch(tbl_dtl(),
      "yrc" = glue("Total multilateral Exports and Imports"),
      "yrpc" = glue("Total bilateral Exports and Imports")
    )
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
  output$exp_col_dtl_yr <- renderHighchart({
    exp_col_dtl_yr()
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
  output$imp_col_dtl_yr <- renderHighchart({
    imp_col_dtl_yr()
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

  dwn_stl <- eventReactive(input$go, {
    "Download country data"
  })

  dwn_txt <- eventReactive(input$go, {
    "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
  })

  dwn_fmt <- eventReactive(input$go, {
    selectInput(
      "fmt",
      "Format",
      choices = available_formats(),
      selected = NULL,
      selectize = TRUE
    )
  })

  output$dwn_dtl_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_dtl(), filename)
    },
    contentType = "application/zip"
  )

  output$dwn_agg_pre <- downloadHandler(
    filename = function() {
      glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
    },
    content = function(filename) {
      export(df_agg(), filename)
    },
    contentType = "application/zip"
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
    downloadButton("dwn_dtl_pre", label = "Detailed data")
  })

  output$dwn_agg <- renderUI({
    req(input$go)
    downloadButton("dwn_agg_pre", label = "Aggregated data")
  })

  ## Citation ----

  site_url <- "https://shiny.tradestatistics.io"

  output$citation_text <- renderUI({
    HTML(
      glue(
        "Open Trade Statistics. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>.
        Accessed {months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }. { site_url }/."
      )
    )
  })

  output$citation_bibtex <- renderUI({
    pre(
      glue("@misc{{open_trd_statistics_{year(Sys.Date())},
      title = {{Open Trade Statistics Beta Dashboard}},
      url = {{{site_url}}},
      author = {{Vargas, Mauricio}},
      doi = {{10.5281/zenodo.3738793}},
      publisher = {{Open Trade Statistics}},
      year = {{2022}},
      month = {{Apr}},
      note = {{Accessed: { months(Sys.Date()) } { day(Sys.Date()) }, { year(Sys.Date()) }}}}}")
    )
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
