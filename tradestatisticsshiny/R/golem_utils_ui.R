available_yrs <- function() {
  1988L:2023L
}

available_yrs_deflator <- function() {
  1989L:2023L
}

available_yrs_min <- function() {
  min(available_yrs())
}

available_yrs_max <- function() {
  max(available_yrs())
}

col_12 <- function(...) {
  column(12, ...)
}

col_10 <- function(...) {
  column(10, ...)
}

col_9 <- function(...) {
  column(9, ...)
}

col_8 <- function(...) {
  column(8, ...)
}

col_6 <- function(...) {
  column(6, ...)
}

col_4 <- function(...) {
  column(4, ...)
}

col_3 <- function(...) {
  column(3, ...)
}

col_2 <- function(...) {
  column(2, ...)
}

col_1 <- function(...) {
  column(1, ...)
}
