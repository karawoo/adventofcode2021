#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' 
#' The incredible pressures at this depth are starting to put a strain on
#' your submarine. The submarine has
#' [polymerization](https://en.wikipedia.org/wiki/Polymerization) equipment
#' that would produce suitable materials to reinforce the submarine, and
#' the nearby volcanically-active caves should even have the necessary
#' input elements in sufficient quantities.
#' 
#' The submarine manual contains [instructions]{title="HO
#' 
#' HO -> OH"} for finding the optimal polymer formula; specifically, it
#' offers a *polymer template* and a list of *pair insertion* rules (your
#' puzzle input). You just need to work out what polymer would result after
#' repeating the pair insertion process a few times.
#' 
#' For example:
#' 
#'     NNCB
#' 
#'     CH -> B
#'     HH -> N
#'     CB -> H
#'     NH -> C
#'     HB -> C
#'     HC -> B
#'     HN -> C
#'     NN -> C
#'     BH -> H
#'     NC -> B
#'     NB -> B
#'     BN -> B
#'     BB -> N
#'     BC -> B
#'     CC -> N
#'     CN -> C
#' 
#' The first line is the *polymer template* - this is the starting point of
#' the process.
#' 
#' The following section defines the *pair insertion* rules. A rule like
#' `AB -> C` means that when elements `A` and `B` are immediately adjacent,
#' element `C` should be inserted between them. These insertions all happen
#' simultaneously.
#' 
#' So, starting with the polymer template `NNCB`, the first step
#' simultaneously considers all three pairs:
#' 
#' -   The first pair (`NN`) matches the rule `NN -> C`, so element `C` is
#'     inserted between the first `N` and the second `N`.
#' -   The second pair (`NC`) matches the rule `NC -> B`, so element `B` is
#'     inserted between the `N` and the `C`.
#' -   The third pair (`CB`) matches the rule `CB -> H`, so element `H` is
#'     inserted between the `C` and the `B`.
#' 
#' Note that these pairs overlap: the second element of one pair is the
#' first element of the next pair. Also, because all pairs are considered
#' simultaneously, inserted elements are not considered to be part of a
#' pair until the next step.
#' 
#' After the first step of this process, the polymer becomes `NCNBCHB`.
#' 
#' Here are the results of a few steps using the above rules:
#' 
#'     Template:     NNCB
#'     After step 1: NCNBCHB
#'     After step 2: NBCCNBBBCBHCB
#'     After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#'     After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#' 
#' This polymer grows quickly. After step 5, it has length 97; After step
#' 10, it has length 3073. After step 10, `B` occurs 1749 times, `C` occurs
#' 298 times, `H` occurs 191 times, and `N` occurs 865 times; taking the
#' quantity of the most common element (`B`, 1749) and subtracting the
#' quantity of the least common element (`H`, 161) produces
#' `1749 - 161 = 1588`.
#' 
#' Apply 10 steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @field template Starting template
#' @export
#' @examples
#' \dontrun{
#' p <- Polymer$new()
#' p$take_steps(10)
#' p$get_answer()
#' }
Polymer <- R6::R6Class("Polymer", list(
  template = readLines(
    system.file("input14.txt", package = "adventofcode2021"),
    n = 1
  ),
  fun_call = {
    x <- readLines(system.file("input14.txt", package = "adventofcode2021"))
    x <- x[3:length(x)]
    src <- substr(x, 1, 2)
    dest <- paste0(substr(x, 1, 1), substr(x, 7, 7), substr(x, 2, 2))
    subs <- setNames(as.list(c(quote(tmp), dest)), c("EXPR", src))
    rlang::call2(quote(switch), !!!subs)
  },

  #' @description take one step
  step = function() {
    res <- vector(mode = "character", length = nchar(self$template) - 1)
    for (i in seq_len(nchar(self$template))[-nchar(self$template)]) {
      tmp <- substr(self$template, i, i + 1)
      if (i == 1) {
        res[i] <- rlang::eval_bare(self$fun_call)
      } else {
        res[i] <- sub(".", "", rlang::eval_bare(self$fun_call))
      }
    }
    self$template <- paste(res, collapse = "")
    invisible(self)
  },

  #' @description take n steps
  #' @param n number of steps to take
  take_steps = function(n = 10) {
    replicate(n = n, self$step())
    invisible(self)
  },

  #' @description Calculate count of most common letter minus least common letter
  get_answer = function() {
    tab <- table(stringr::str_split(self$template, "", nchar(self$template)))
    max(tab) - min(tab)
  }
))
