#' Available Server parameters expressed as functions
#' @export
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

#' SQL connection
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' @export
sql_con <- function() {
  con <- dbPool(
    drv = Postgres(),
    dbname = "tradestatistics",
    host = "localhost",
    user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
  )

  return(con)
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
#' @importFrom highcharter JS
#' @export
custom_tooltip <- function() {
  JS("function() { return '<b>' + this.name + '</b>' + '<br>' +
                        'Share: ' + Math.round(this.value / this.series.tree.val * 10000)/100 + '%' + '<br>' +
                        'Value: ' + Highcharts.numberFormat(this.value, 0) + ' USD'
                        }")
}

#' Custom Short Tooltip (For Highcharter Visuals)
#' @importFrom highcharter JS
#' @export
custom_tooltip_short <- function() {
  JS("function() { return '<b>' + this.series.name + '</b>' + ' ' +
     Highcharts.numberFormat(this.y, 0) + ' USD' }")
}

#' Custom Data Labels (For Highcharter Visuals)
#' @importFrom highcharter JS
#' @export
data_labels <- function() {
  JS("function() { return this.key + '<br>' + Math.round(this.point.value / this.point.series.tree.val * 10000 ) / 100 + '%'}")
}

#' Origin-Destination to Highcharter (For Highcharter Visuals)
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @importFrom dplyr arrange desc distinct group_by mutate pull summarise ungroup
#' @importFrom highcharter data_to_hierarchical hchart
#' @export
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
#' @importFrom dplyr arrange distinct filter group_by mutate pull
#'     select summarise ungroup
#' @importFrom purrr map_df
#' @export
p_aggregate_by_section <- function(d, col, con) {
  d <- d %>%
    select(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_color"), trade_value = !!sym(col))

  d <- p_aggregate_products(d, con = con)

  d <- d %>%
    # ensure we preserve commodity_code_short when available via p_aggregate_products
    group_by(!!sym("section_code"), !!sym("section_color"), !!sym("section_name"), !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("commodity_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup()

  d <- d %>%
    select(!!sym("section_code"), !!sym("section_color"), !!sym("section_name"), !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("commodity_name"), !!sym("trade_value")) %>%
    group_by(!!sym("section_code"), !!sym("section_color"), !!sym("section_name")) %>%
    mutate(sum_trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    arrange(-!!sym("sum_trade_value")) %>%
    select(-!!sym("sum_trade_value"))

  d <- map_df(
    d %>%
      select(!!sym("section_code")) %>%
      distinct() %>%
      pull(),
    function(s) {
      d %>%
        filter(!!sym("section_code") == s) %>%
        arrange(-!!sym("trade_value"))
    }
  )

  return(d)
}

#' Aggregate Products (For Highcharter Visuals)
#' @param d input dataset
#' @param con SQL connection
#' @importFrom dplyr collect filter group_by left_join mutate select
#'     summarise tbl ungroup
#' @export
p_aggregate_products <- function(d, con) {
  d %>%
    inner_join(
      tbl(con, "commodities") %>%
        select(!!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"),
          !!sym("section_color"), !!sym("section_name")) %>%
        collect()
    ) %>%
    group_by(
      !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"), 
      !!sym("section_color"), !!sym("section_name")
    ) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    left_join(
      tbl(con, "commodities_short") %>%
        select(!!sym("commodity_code"),
          commodity_name = !!sym("commodity_name")
        ) %>%
        collect(),
      by = c("commodity_code_short" = "commodity_code")
    )
}

#' Product to Highcharter (For Highcharter Visuals)
#' @param d input dataset for values
#' @param d2 optional dataset for colours (when provided it will be used as the
#' color reference so the same `section_name` maps to the same colour across
#' different treemaps)
#' @importFrom dplyr collect distinct inner_join mutate_if select tbl arrange pull slice
#' @importFrom rlang sym
#' @export
p_to_highcharts <- function(d, d2 = NULL) {
  # ensure section_name (and section_code if present) are character
  dd <- d %>%
    mutate(section_name = as.character(section_name))
  if ("section_code" %in% names(dd)) dd <- dd %>% mutate(section_code = as.character(section_code))

  # Build reference of sections and colours. Prefer section_code when available.
  if (!is.null(d2)) {
    if ("section_code" %in% names(d2)) {
      ref <- d2 %>%
        distinct(section_code, section_name, section_color) %>%
        mutate(section_code = as.character(section_code), section_name = as.character(section_name))
      use_code <- TRUE
    } else {
      ref <- d2 %>%
        distinct(section_name, section_color) %>%
        mutate(section_name = as.character(section_name))
      use_code <- FALSE
    }
  } else {
    if ("section_code" %in% names(dd)) {
      ref <- dd %>%
        distinct(section_code, section_name, section_color) %>%
        mutate(section_code = as.character(section_code), section_name = as.character(section_name))
      use_code <- TRUE
    } else {
      ref <- dd %>%
        distinct(section_name, section_color) %>%
        mutate(section_name = as.character(section_name))
      use_code <- FALSE
    }
  }

  # Create mapping helpers and derive ordered colors vector.
  norm_func <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))

  # If a colour reference (d2) was provided prefer its order. Use section_code
  # when available for exact matching; fall back to name-based ordering.
  if (use_code) {
    # preserve order of ref as provided in d2
    ref <- ref %>% mutate(section_code = as.character(section_code))
    code_color_map <- setNames(as.character(ref$section_color), as.character(ref$section_code))

    # build section order as intersection of ref codes and observed dd codes
    observed_codes <- unique(dd$section_code)
    section_order <- ref$section_code[ref$section_code %in% observed_codes]

    colors_for_dd <- if (length(section_order) > 0) {
      unname(code_color_map[section_order])
    } else {
      character(0)
    }
  } else {
    # name-based mapping: compute ordering by total trade (existing behaviour)
    color_map <- setNames(as.character(ref$section_color), norm_func(ref$section_name))

    section_levels <- dd %>%
      group_by(!!sym("section_name")) %>%
      summarise(total = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(!!sym("total"))) %>%
      pull(!!sym("section_name"))

    dd <- dd %>%
      mutate(section_name = factor(!!sym("section_name"), levels = section_levels)) %>%
      arrange(!!sym("section_name"))

    section_order_df <- dd %>%
      distinct(section_name, section_color) %>%
      mutate(section_name = as.character(section_name)) %>%
      slice(match(section_levels, section_name))

    colors_for_dd <- if (nrow(section_order_df) > 0) section_order_df$section_color else character(0)
  }

  # If we have a stable section_code we can build an explicit flat id/parent
  # treemap dataset (id, parent, name, value, color). This is deterministic
  # and ensures colours come from `ref` (d2) in the requested order. If
  # section_code is not available we fall back to the previous
  # data_to_hierarchical + heuristic colour assignment.
  if (use_code) {
    # Summarise section totals and children (commodities)
    sections_df <- dd %>%
      group_by(!!sym("section_code"), !!sym("section_name"), !!sym("section_color")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop")

    # Respect the order from ref (d2) but keep only observed sections
    section_order <- ref$section_code[ref$section_code %in% sections_df[["section_code"]]]
    sections_df <- sections_df %>%
      mutate(section_code = factor(!!sym("section_code"), levels = section_order)) %>%
      arrange(!!sym("section_code")) %>%
      mutate(section_code = as.character(!!sym("section_code")))

    children_df <- dd %>%
      group_by(!!sym("section_code"), !!sym("commodity_name")) %>%
      summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop")

    # Build list-of-lists points for highcharter: top-level sections + children
    section_points <- purrr::pmap(
      list(sections_df[["section_code"]], sections_df[["section_name"]], sections_df[["trade_value"]], sections_df[["section_color"]]),
      function(id, name, value, color) {
        list(id = as.character(id), name = as.character(name), value = as.numeric(value), color = as.character(color))
      }
    )

    child_points <- purrr::pmap(
      list(children_df[["section_code"]], children_df[["commodity_name"]], children_df[["trade_value"]]),
      function(parent, name, value) {
        # use parent colour for children so they remain visually grouped
        parent_col <- ref$section_color[match(parent, ref$section_code)]
        list(id = paste0(as.character(parent), "__", as.character(name)), name = as.character(name), parent = as.character(parent), value = as.numeric(value), color = as.character(parent_col))
      }
    )

    points <- c(section_points, child_points)

    els <- list(series = list(list(data = points)))

    # Temporary debug: save the inputs and constructed points to /tmp for
    # inspection when running the app locally. This will help verify that
    # colours and ids are constructed as expected.
    try({
      dbg <- list(d = dd, d2 = ref, sections = sections_df, children = children_df, points = points)
      fn <- file.path("/tmp", paste0("p_to_highcharts_debug_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
      saveRDS(dbg, fn)
    }, silent = TRUE)
  } else {
    # Build a temporary section key that embeds section_code when available. This
    # lets us build hierarchical nodes keyed by stable code but still display
    # human labels later.
    dd <- dd %>% mutate(section_key = section_name)

    # Build hierarchical via data_to_hierarchical using section_key so top-level
    # nodes contain the code token we can parse out later.
    els <- data_to_hierarchical(dd, c("section_key", "commodity_name"), "trade_value",
      colors = colors_for_dd
    )

    assign_colors_to_nodes <- function(node) {
      if (!is.list(node)) return(node)
      if (!is.null(node$name)) {
        n_raw <- as.character(node$name)
        n <- norm_func(n_raw)
        if (exists("color_map") && n %in% names(color_map)) {
          node$color <- color_map[[n]]
        } else {
          matched <- NULL
          if (exists("color_map")) {
            for (k in names(color_map)) {
              if (grepl(k, n, fixed = TRUE) || grepl(n, k, fixed = TRUE)) {
                matched <- k
                break
              }
            }
          }
          if (!is.null(matched)) node$color <- color_map[[matched]]
        }
      }

      if (!is.null(node$data) && is.list(node$data)) node$data <- lapply(node$data, assign_colors_to_nodes)
      if (!is.null(node$children) && is.list(node$children)) node$children <- lapply(node$children, assign_colors_to_nodes)
      return(node)
    }

    if (!is.null(els$series) && is.list(els$series)) {
      els$series <- lapply(els$series, function(s) {
        if (!is.null(s$data) && is.list(s$data)) s$data <- lapply(s$data, assign_colors_to_nodes) else s <- assign_colors_to_nodes(s)
        s
      })
    } else {
      els <- assign_colors_to_nodes(els)
    }
  }

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

# Small grammar helpers used by glue templates in the app_server
# These used to live in the app namespace; define safe fallbacks here so
# glues don't fail when the functions are referenced during rendering.
#' @export
r_add_the <- function(name = NULL) {
  # Return 'the' for reporter names that typically take the article
  if (is.null(name)) return("")
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

#' @export
r_add_upp_the <- function(name = NULL) {
  v <- r_add_the(name)
  if (nchar(v) == 0) return("")
  # Capitalize only the first letter ("The") rather than returning all caps
  paste0(toupper(substr(v, 1, 1)), tolower(substr(v, 2, nchar(v))))
}

#' @export
p_add_the <- function(name = NULL) {
  # same logic for partner names
  if (is.null(name)) return("")
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

# FORMAT TEXTS ----

#' Format for Dollars
#' @param x input number
#' @export
show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
    paste0(round(x / 10e8, 2), "B"),
    paste0(round(x / 10e5, 2), "M")
  )
}

#' Format for Percentages
#' @param x inout number
#' @export
show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

#' Compute Compound Annualized Growth Rate
#' @param p final value
#' @param q initial value
#' @param t time period
#' @export
growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

#' Typing reactiveValues is too long
#' @param ... elements to pass to the function
#' @rdname reactives
#' @export
rv <- function(...) shiny::reactiveValues(...)

#' @rdname reactives
#' @export
rvtl <- function(...) shiny::reactiveValuesToList(...)
