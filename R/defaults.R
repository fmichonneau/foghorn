##' @importFrom tibble tibble
default_cran_checks <- tibble::tibble(
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0),
    FAIL = integer(0)
)


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
                            word = "other issues")
)
