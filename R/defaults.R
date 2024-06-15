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
##' @importFrom cli col_red col_magenta col_yellow col_blue col_cyan symbol
foghorn_components <- list(
  `error` = c(
    symbol = cli::symbol$cross,
    color = cli::col_red,
    word = "errors"
  ),
  `fail` = c(
    symbol = cli::symbol$cross,
    color = cli::col_magenta,
    word = "fails"
  ),
  `warn` = c(
    symbol = cli::symbol$warning,
    color = cli::col_yellow,
    word = "warnings"
  ),
  `note` = c(
    symbol = cli::symbol$star,
    color = cli::col_blue,
    word = "notes"
  ),
  `has_other_issues` = c(
    symbol = cli::symbol$circle_filled,
    color = cli::col_cyan,
    word = "other issues"
  ),
  `other_issue` = c(
    symbol = cli::symbol$circle_filled,
    color = cli::col_cyan,
    word = "additional issues"
  )
)
