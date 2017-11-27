##' @importFrom tibble tibble
default_cran_results <- tibble::tibble(
    package = character(0),
    note = integer(0),
    ok = integer(0),
    warn = integer(0),
    error = integer(0),
    fail = integer(0)
    )

default_cran_details <- tibble::tibble(
   package = character(0),
   version = character(0),
   result = character(0),
   check = character(0),
   flavors = character(0),
   n_flavors = integer(0),
   message = character(0)
)


##' @importFrom tibble tibble
##' @importFrom clisymbols symbol
##' @importFrom crayon red magenta yellow blue cyan
foghorn_components <- list(
    `error` = c(symbol = clisymbols::symbol$cross,
                color = crayon::red,
                word = "errors"
                ),
    `fail` = c(symbol = clisymbols::symbol$cross,
               color = crayon::magenta,
               word = "fails"
               ),
    `warn` = c(symbol = clisymbols::symbol$warning,
               color = crayon::yellow,
               word = "warnings"),
    `note` = c(symbol = clisymbols::symbol$star,
               color = crayon::blue,
               word = "notes"),
    `has_other_issues` = c(symbol = clisymbols::symbol$circle_filled,
                            color = crayon::cyan,
                           word = "other issues"),
    `other_issue` = c(symbol = clisymbols::symbol$circle_filled,
                       color = crayon::cyan,
                       word = "additional issues")
)
