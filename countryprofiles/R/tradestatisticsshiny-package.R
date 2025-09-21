#' @keywords internal
"_PACKAGE"

styles <- list(
  skin_color = "blue",
  css_files = "custom.css"
)

shiny::shinyOptions(
  cache = cachem::cache_disk(
    dir = "/tradestatistics/cache"
    # logfile = "/tradestatistics/log/cache.log"
  )
)

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
