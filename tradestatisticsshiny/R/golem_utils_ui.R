available_yrs <- function() {
  1988L:2023L
}

available_yrs_deflator <- function() {
  available_yrs()
}

available_yrs_min <- function() {
  min(available_yrs())
}

available_yrs_max <- function() {
  max(available_yrs())
}

#' @title Shorthand for column of width 12
col_12 <- function(...) {
  column(12, ...)
}

#' @title Shorthand for column of width 11
col_11 <- function(...) {
  column(11, ...)
}

#' @title Shorthand for column of width 10
col_10 <- function(...) {
  column(10, ...)
}

#' @title Shorthand for column of width 9
col_9 <- function(...) {
  column(9, ...)
}

#' @title Shorthand for column of width 8
col_8 <- function(...) {
  column(8, ...)
}

#' @title Shorthand for column of width 7
col_7 <- function(...) {
  column(7, ...)
}

#' @title Shorthand for column of width 6
col_6 <- function(...) {
  column(6, ...)
}

#' @title Shorthand for column of width 5
col_5 <- function(...) {
  column(5, ...)
}

#' @title Shorthand for column of width 4
col_4 <- function(...) {
  column(4, ...)
}

#' @title Shorthand for column of width 3
col_3 <- function(...) {
  column(3, ...)
}

#' @title Shorthand for column of width 2
col_2 <- function(...) {
  column(2, ...)
}

#' @title Shorthand for column of width 1
col_1 <- function(...) {
  column(1, ...)
}
