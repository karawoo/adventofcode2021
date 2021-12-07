#' Day 02: Dive!
#'
#' [Dive!](https://adventofcode.com/2021/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' 
#' Now, you need to figure out how to [pilot this
#' thing]{title="Tank, I need a pilot program for a B212 helicopter."}.
#' 
#' It seems like the submarine can take a series of commands like
#' `forward 1`, `down 2`, or `up 3`:
#' 
#' -   `forward X` increases the horizontal position by `X` units.
#' -   `down X` *increases* the depth by `X` units.
#' -   `up X` *decreases* the depth by `X` units.
#' 
#' Note that since you\'re on a submarine, `down` and `up` affect your
#' *depth*, and so they have the opposite result of what you might expect.
#' 
#' The submarine seems to already have a planned course (your puzzle
#' input). You should probably figure out where it\'s going. For example:
#' 
#'     forward 5
#'     down 5
#'     forward 8
#'     up 3
#'     down 8
#'     forward 2
#' 
#' Your horizontal position and depth both start at `0`. The steps above
#' would then modify them as follows:
#' 
#' -   `forward 5` adds `5` to your horizontal position, a total of `5`.
#' -   `down 5` adds `5` to your depth, resulting in a value of `5`.
#' -   `forward 8` adds `8` to your horizontal position, a total of `13`.
#' -   `up 3` decreases your depth by `3`, resulting in a value of `2`.
#' -   `down 8` adds `8` to your depth, resulting in a value of `10`.
#' -   `forward 2` adds `2` to your horizontal position, a total of `15`.
#' 
#' After following these instructions, you would have a horizontal position
#' of `15` and a depth of `10`. (Multiplying these together produces
#' `150`.)
#' 
#' Calculate the horizontal position and depth you would have after
#' following the planned course. *What do you get if you multiply your
#' final horizontal position by your final depth?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b(example_data_02())
f02a <- function(x) {
  h <- sum(as.numeric(gsub("forward (\\d)", "\\1", x)[grepl("forward", x)]))
  d <- sum(as.numeric(gsub("down (\\d)", "\\1", x)[grepl("down", x)]))
  u <- sum(as.numeric(gsub("up (\\d)", "\\1", x)[grepl("up", x)]))
  h * (d - u)
}

#' @rdname day02
#' @export
f02b <- function(x) {
  aim <- 0
  horiz <- 0
  depth <- 0
  for (i in strsplit(x, " ")) {
    if (i[[1]] == "down") aim <- aim + as.numeric(i[[2]])
    if (i[[1]] == "up") aim <- aim - as.numeric(i[[2]])
    if (i[[1]] == "forward") {
      horiz <- horiz + as.numeric(i[[2]])
      depth <- depth + (as.numeric(i[[2]]) * aim)
    }
  }
  horiz * depth
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  readLines(system.file("input02.txt", package = "adventofcode2021"))
}

# R6 solution ------------------------------------------------------------------

#' Model the position of a submarine
#'
#' @field position horizontal position
#' @field depth vertical depth
#'
#' @export
#' @examples
#' dat <- "forward 5
#' down 5
#' forward 8
#' up 3
#' down 8
#' forward 2"
#' x <- strsplit(dat, "\n")[[1]]
#' s <- Submarine$new()
#' s$move(x)
Submarine <- R6::R6Class("Submarine", list(
  position = 0,
  depth = 0,

  #' @description Move forward
  #' @param x forward distance to add
  forward = function(x = 1) {
    self$position <- self$position + x
    invisible(self)
  },

  #' @description Move down
  #' @param x depth to add
  down = function(x = 1) {
    self$depth <- self$depth + x
    invisible(self)
  },

  #' @description Move up
  #' @param x depth to subtraxt
  up = function(x = 1) {
    self$depth <- self$depth - x
    invisible(self)
  },

  #' @description Move based on a set of commands
  #' @param commands vector of commands such as "forward 1", "down 2"
  move = function(commands) {
    purrr::walk(commands, function(x) {
      fn <- strsplit(x, " ")[[1]]
      do.call(self[[fn[[1]]]], list(as.numeric(fn[[2]])))
    })
    return(self$position * self$depth)
  }
))
