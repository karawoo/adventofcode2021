#' Day 03: Binary Diagnostic
#'
#' [Binary Diagnostic](https://adventofcode.com/2021/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' 
#' The submarine has been making some [odd creaking
#' noises]{title="Turns out oceans are heavy."}, so you ask it to produce a
#' diagnostic report just in case.
#' 
#' The diagnostic report (your puzzle input) consists of a list of binary
#' numbers which, when decoded properly, can tell you many useful things
#' about the conditions of the submarine. The first parameter to check is
#' the *power consumption*.
#' 
#' You need to use the binary numbers in the diagnostic report to generate
#' two new binary numbers (called the *gamma rate* and the *epsilon rate*).
#' The power consumption can then be found by multiplying the gamma rate by
#' the epsilon rate.
#' 
#' Each bit in the gamma rate can be determined by finding the *most common
#' bit in the corresponding position* of all numbers in the diagnostic
#' report. For example, given the following diagnostic report:
#' 
#'     00100
#'     11110
#'     10110
#'     10111
#'     10101
#'     01111
#'     00111
#'     11100
#'     10000
#'     11001
#'     00010
#'     01010
#' 
#' Considering only the first bit of each number, there are five `0` bits
#' and seven `1` bits. Since the most common bit is `1`, the first bit of
#' the gamma rate is `1`.
#' 
#' The most common second bit of the numbers in the diagnostic report is
#' `0`, so the second bit of the gamma rate is `0`.
#' 
#' The most common value of the third, fourth, and fifth bits are `1`, `1`,
#' and `0`, respectively, and so the final three bits of the gamma rate are
#' `110`.
#' 
#' So, the gamma rate is the binary number `10110`, or `22` in decimal.
#' 
#' The epsilon rate is calculated in a similar way; rather than use the
#' most common bit, the least common bit from each position is used. So,
#' the epsilon rate is `01001`, or `9` in decimal. Multiplying the gamma
#' rate (`22`) by the epsilon rate (`9`) produces the power consumption,
#' `198`.
#' 
#' Use the binary numbers in your diagnostic report to calculate the gamma
#' rate and epsilon rate, then multiply them together. *What is the power
#' consumption of the submarine?* (Be sure to represent your answer in
#' decimal, not binary.)
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b(example_data_03())
f03a <- function(x) {
  gamma <- strtoi(
    paste0(apply(x, MARGIN = 2, most_common), collapse = ""),
    base = 2
  )
  epsilon <- strtoi(
    paste0(apply(x, MARGIN = 2, least_common), collapse = ""),
    base = 2
  )
  gamma * epsilon
}

#' @rdname day03
#' @export
f03b <- function(x) {
  oxygen <- x
  co2 <- x

  i <- 1
  while(nrow(oxygen) > 1) {
    val <- most_common(oxygen[, i])
    oxygen <- oxygen[oxygen[, i] == val, ]
    i <- i + 1
  }

  i <- 1
  while(nrow(co2) > 1) {
    val <- least_common(co2[, i])
    co2 <- co2[co2[, i] == val, ]
    i <- i + 1
  }

  oxygen <- strtoi(paste0(oxygen, collapse = ""), base = 2)
  co2 <- strtoi(paste0(co2, collapse = ""), base = 2)
  oxygen * co2
}

most_common <- function(x) {
  x <- table(x)
  as.numeric(names(utils::tail(x[which(x == max(x))], 1)))
}

least_common <- function(x) {
  x <- table(x)
  as.numeric(names(utils::head(x[which(x == min(x))], 1)))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  utils::read.fwf(
    system.file("input03.txt", package = "adventofcode2021"),
    widths = rep(1, 12)
  )
}
