#' Available Server parameters expressed as functions
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

#' SQL connection
sql_con <- function() {
  dbPool(
    drv = Postgres(),
    dbname = "tradestatistics",
    host = "localhost",
    user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
  )
}

# ORIGIN/DESTINATION TREEMAPS -----

lvl_opts <- list(
  list(
    level = 1,
    borderWidth = 1,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.25),
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(
        fontSize = "14px",
        fontWeight = "600",
        textOutline = FALSE
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 1,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 1),
    dataLabels = list(enabled = FALSE),
    style = list(
      fontSize = "14px",
      fontWeight = "600",
      textOutline = FALSE
    )
  )
)


#' Custom Tooltip (For Highcharter Visuals)
custom_tooltip <- function() {
  JS("function() {\n                var raw = (this.point && this.point.raw_value != null) ? this.point.raw_value : this.value;\n                return '<b>' + this.name + '</b>' + '<br>' +\n                       'Share: ' + Math.round(raw / this.series.tree.val * 10000)/100 + '%' + '<br>' +\n                       'Value: ' + Highcharts.numberFormat(raw, 0) + ' USD'\n                       }")
}

#' Custom Short Tooltip (For Highcharter Visuals)
custom_tooltip_short <- function() {
  JS("function() { return '<b>' + this.series.name + '</b>' + ' ' +
     Highcharts.numberFormat(this.y, 0) + ' USD' }")
}

#' Custom Data Labels (For Highcharter Visuals)
data_labels <- function() {
  JS("function() { return this.key + '<br>' + Math.round(this.point.value / this.point.series.tree.val * 10000 ) / 100 + '%'}")
}

#' Origin-Destination to Highcharter (For Highcharter Visuals)
#' @param d input dataset for values
#' @param d2 input dataset for colours
od_to_highcharts <- function(d, d2) {
  dd <- d %>%
    mutate(continent_name = factor(!!sym("continent_name"), levels = d2$continent_name)) %>%
    arrange(!!sym("continent_name"))

  new_lvls <- dd %>%
    group_by(!!sym("continent_name"), !!sym("country_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("continent_name")) %>%
    pull(!!sym("continent_name"))

  new_colors <- d2 %>%
    mutate(continent_name = factor(!!sym("continent_name"),
      levels = new_lvls
    )) %>%
    arrange(!!sym("continent_name")) %>%
    pull(!!sym("country_color"))

  els <- data_to_hierarchical(dd, c("continent_name", "country_name"), "trade_value",
    colors = new_colors
  )

  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)

  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(pointFormatter = custom_tooltip()),
    dataLabels = list(formatter = data_labels())
  )
}

# PRODUCT TREEMAPS ----

#' Add Percentages to Sections (For Highcharter Visuals)
#' @param d input dataset
#' @param col column to collapse
#' @param con SQL connection
p_aggregate_by_section <- function(d, col, con) {
  d <- d %>%
    select(!!sym("commodity_code"), !!sym("section_code"), !!sym(col)) %>%
    rename(trade_value = !!sym(col))

  d <- p_aggregate_products(d, con = con)

  d <- d %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T), .groups = "drop") %>%
    ungroup()

  d <- d %>%
    select(!!sym("section_name"), !!sym("commodity_name"), !!sym("trade_value")) %>%
    group_by(!!sym("section_name")) %>%
    mutate(sum_trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    arrange(-!!sym("sum_trade_value")) %>%
    select(-!!sym("sum_trade_value"))

  d <- map_df(
    d %>%
      select(!!sym("section_name")) %>%
      distinct() %>%
      pull(),
    function(s) {
      d %>%
        filter(!!sym("section_name") == s) %>%
        arrange(-!!sym("trade_value"))
    }
  )

  return(d)
}

#' Colorize Products (For Highcharter Visuals)
#' @param d input dataset
#' @param con SQL connection
p_colors <- function(d, con) {
  d %>%
    distinct(!!sym("section_name")) %>%
    inner_join(
      tbl(con, "commodities") %>%
        select(
          !!sym("section_code"),
          !!sym("section_name"),
          !!sym("section_color")
        ) %>%
        distinct() %>%
        collect(),
      by = "section_name"
    )
}

#' Aggregate Products (For Highcharter Visuals)
#' @param d input dataset
#' @param con SQL connection
p_aggregate_products <- function(d, con) {
  # Get all commodity data in one query to avoid multiple database hits
  commodities_data <- tbl(con, "commodities") %>%
    select(
      !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"),
      !!sym("section_color"), !!sym("section_name")
    ) %>%
    left_join(
      tbl(con, "commodities_short") %>%
        select(!!sym("commodity_code"),
          commodity_name = !!sym("commodity_name")
        ),
      by = c("commodity_code_short" = "commodity_code")
    ) %>%
    collect()

  d %>%
    inner_join(commodities_data, by = c("commodity_code", "section_code")) %>%
    group_by(
      !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"),
      !!sym("section_color"), !!sym("section_name"), !!sym("commodity_name")
    ) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T), .groups = "drop") %>%
    ungroup()
}

#' Product to Highcharter (For Highcharter Visuals)
#' @param d input dataset for values
#' @param d2 input dataset for colours
p_to_highcharts <- function(d, d2) {
  dd <- d %>%
    mutate(section_name = factor(!!sym("section_name"), levels = d2$section_name)) %>%
    arrange(!!sym("section_name"))

  new_lvls <- dd %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(
      trade_value = sum(!!sym("trade_value"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("section_name")) %>%
    pull(!!sym("section_name"))

  new_colors <- d2 %>%
    mutate(section_name = factor(!!sym("section_name"), levels = new_lvls)) %>%
    arrange(!!sym("section_name")) %>%
    pull(!!sym("section_color"))

  els <- data_to_hierarchical(dd, c("section_name", "commodity_name"), "trade_value",
    colors = new_colors
  )

  lopts <- getOption("highcharter.lang")
  lopts$thousandsSep <- ","
  options(highcharter.lang = lopts)

  hchart(
    els,
    type = "treemap",
    levelIsConstant = FALSE,
    allowDrillToNode = TRUE,
    levels = lvl_opts,
    tooltip = list(
      pointFormatter = custom_tooltip()
    ),
    dataLabels = list(
      formatter = data_labels()
    )
  )
}

#' Add definite article for reporter names
#'
#' Grammar helper function that adds "the" for reporter names that typically
#' take the definite article (e.g., "United States", "Russian Federation").
#'
#' @param name Character string of the reporter name
#' @return Character string: "the" for names requiring the article, empty string otherwise
r_add_the <- function(name = NULL) {
  # Return 'the' for reporter names that typically take the article
  if (is.null(name)) {
    return("")
  }
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

#' Add capitalized definite article for reporter names
#'
#' Grammar helper function that adds "The" (capitalized) for reporter names that
#' typically take the definite article, used at the beginning of sentences.
#'
#' @param name Character string of the reporter name
#' @return Character string: "The" for names requiring the article, empty string otherwise
r_add_upp_the <- function(name = NULL) {
  v <- r_add_the(name)
  if (nchar(v) == 0) {
    return("")
  }
  # Capitalize only the first letter ("The") rather than returning all caps
  paste0(toupper(substr(v, 1, 1)), tolower(substr(v, 2, nchar(v))))
}

#' Add definite article for partner names
#'
#' Grammar helper function that adds "the" for partner names that typically
#' take the definite article (e.g., "United States", "Russian Federation").
#'
#' @param name Character string of the partner name
#' @return Character string: "the" for names requiring the article, empty string otherwise
p_add_the <- function(name = NULL) {
  # same logic for partner names
  if (is.null(name)) {
    return("")
  }
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

# FORMAT TEXTS ----

#' Format for Dollars
#' @param x input number
show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
    paste0(round(x / 10e8, 2), "B"),
    paste0(round(x / 10e5, 2), "M")
  )
}

#' Format for Percentages
#' @param x input number
show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

#' Compute Compound Annualized Growth Rate
#' @param p final value
#' @param q initial value
#' @param t time period
growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

#' Typing reactiveValues is too long
#' @param ... elements to pass to the function
#' @rdname reactives
rv <- function(...) shiny::reactiveValues(...)

#' @rdname reactives
rvtl <- function(...) shiny::reactiveValuesToList(...)
