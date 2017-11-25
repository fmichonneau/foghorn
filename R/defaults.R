##' @importFrom tibble tibble
default_cran_results <- tibble::tibble(
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0),
    FAIL = integer(0)
    )

default_cran_details <- tibble::tibble(
   Package = character(0),
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
    `ERROR` = c(symbol = clisymbols::symbol$cross,
                color = crayon::red,
                word = "errors"
                ),
    `FAIL` = c(symbol = clisymbols::symbol$cross,
               color = crayon::magenta,
               word = "fails"
               ),
    `WARN` = c(symbol = clisymbols::symbol$warning,
               color = crayon::yellow,
               word = "warnings"),
    `NOTE` = c(symbol = clisymbols::symbol$star,
               color = crayon::blue,
               word = "notes"),
    `has_other_issues` = c(symbol = clisymbols::symbol$circle_filled,
                            color = crayon::cyan,
                           word = "other issues"),
    `other_issue` = c(symbol = clisymbols::symbol$circle_filled,
                       color = crayon::cyan,
                       word = "additional issues")
)
