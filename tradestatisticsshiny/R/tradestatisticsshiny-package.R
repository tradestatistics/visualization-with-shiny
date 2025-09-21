#' @keywords internal
#' @importFrom cachem cache_disk
#' @importFrom shiny shinyOptions
"_PACKAGE"

styles <- list(
  skin_color = "blue",
  css_files = "custom.css"
)

shinyOptions(
  cache = cache_disk(
    dir = "/tradestatistics/cache"
    # logfile = "/tradestatistics/log/cache.log"
  )
)

#' Convert dollars from year X to year Y
#' @param d input dataset
#' @param reference_year year to convert dollars
#' @param con SQL connection
#' @importFrom dplyr distinct last pull tibble
#' @importFrom purrr map_df
#' @importFrom rlang sym
#' @export
gdp_deflator_adjustment <- function(d, reference_year, con) {
  years <- d %>%
    distinct(!!sym("year")) %>%
    pull()

  dd <- map_df(
    years,
    function(year) {
      if (year < reference_year) {
        tbl(con, "gdp_deflator") %>%
          filter(!!sym("year_to") <= reference_year &
            !!sym("year_to") > !!sym("year_from") &
            !!sym("country_iso") == "all") %>%
          collect() %>%
          summarise(gdp_deflator = last(cumprod(!!sym("gdp_deflator")))) %>%
          mutate(year = year, conversion_year = reference_year)
      } else if (year > reference_year) {
        tbl(con, "gdp_deflator") %>%
          filter(!!sym("year_from") >= reference_year &
            !!sym("year_to") > !!sym("year_from") &
            !!sym("country_iso") == "all") %>%
          collect() %>%
          summarise(gdp_deflator = 1 / last(cumprod(!!sym("gdp_deflator")))) %>%
          mutate(year = year, conversion_year = reference_year)
      } else if (year == reference_year) {
        tibble(
          year = year, conversion_year = year, gdp_deflator = 1
        )
      }
    }
  )

  d <- d %>%
    left_join(dd, by = "year") %>%
    mutate(
      trade_value_usd_imp = round(!!sym("trade_value_usd_imp") * !!sym("gdp_deflator"), 0),
      trade_value_usd_exp = round(!!sym("trade_value_usd_exp") * !!sym("gdp_deflator"), 0)
    )

  return(d)
}

#' reporters_display
#'
#' Internal dataset for country codes.
#'
#' @docType data
#' @keywords datasets
#' @name reporters_display
NULL

#' sections_display
#'
#' Internal dataset for section/commodity codes.
#'
#' @docType data
#' @keywords datasets
#' @name sections_display
NULL

#' commodities
#'
#' Internal dataset for commodity codes (6,898 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities
NULL

#' commodities_display
#'
#' Internal dataset for commodity codes (6,898 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities_display
NULL

#' commodities_short_display
#'
#' Internal dataset for commodity codes (1,363 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities_short_display
NULL
