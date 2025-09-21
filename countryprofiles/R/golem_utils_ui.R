#' Available UI parameters expressed as functions
#' @rdname available
#' @export
available_all <- function() {
  c("All Products" = "all")
}

#' @rdname available
#' @export
available_logicals <- function() {
  c("Yes" = "yes", "No" = "no")
}

#' @rdname available
#' @export
available_models <- function() {
  c(
    "Poisson Pseudo Maximum Likelihood (PPML)" = "ppml",
    "Ordinary Least Squares (OLS) [Not Recommended]" = "ols"
  )
}

#' @rdname available
#' @export
available_reporters_iso <- function() {
  tradestatisticsshiny::reporters_display
}

#' @rdname available
#' @export
available_sections_code <- function() {
  tradestatisticsshiny::sections_display
}

#' @rdname available
#' @export
available_commodities_code <- function() {
  tradestatisticsshiny::commodities_display
}

#' @rdname available
#' @export
available_commodities_short_code <- function() {
  tradestatisticsshiny::commodities_short_display
}

#' @rdname available
#' @export
available_yrs <- function() {
  1988L:2023L
}

#' @rdname available
#' @export
available_yrs_deflator <- function() {
  1989L:2023L
}

#' @rdname available
#' @export
available_yrs_min <- function() {
  min(available_yrs())
}

#' @rdname available
#' @export
available_yrs_max <- function() {
  max(available_yrs())
}

#' Create an url
#' @param url the URL
#' @param text the text to display
#' @return an a tag
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
#' @export
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#' @param ... additional parameters for a column division
#' @importFrom shiny column
#' @rdname cols
#' @export
col_12 <- function(...) {
  column(12, ...)
}

#' @rdname cols
#' @export
col_10 <- function(...) {
  column(10, ...)
}

#' @rdname cols
#' @export
col_9 <- function(...) {
  column(9, ...)
}

#' @rdname cols
#' @export
col_8 <- function(...) {
  column(8, ...)
}

#' @rdname cols
#' @export
col_6 <- function(...) {
  column(6, ...)
}

#' @rdname cols
#' @export
col_4 <- function(...) {
  column(4, ...)
}

#' @rdname cols
#' @export
col_3 <- function(...) {
  column(3, ...)
}

#' @rdname cols
#' @export
col_2 <- function(...) {
  column(2, ...)
}

#' @rdname cols
#' @export
col_1 <- function(...) {
  column(1, ...)
}
