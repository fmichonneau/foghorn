##' The number of CRAN flavors
##'
##' The CRAN flavors, the systems on which CRAN tests all packages regularly,
##' are listed <https://cran.r-project.org/web/checks/check_flavors.html>. To
##' get the correct results, foghorn needs to know how many flavors CRAN uses.
##' This function reads the number of flavors that CRAN currently uses, and
##' caches it (per session, in the `tempdir()` folder). Arguments control
##' caching, fall back, and default values.
##'
##' The default values for the arguments are read from options. Given that
##' `n_cran_flavors` function is relied on internally to provide accurate
##' information to the user, using options allows you to control how the
##' function behaves directly. In general, the default values should not be
##' changed. They are provided in case you have issues connecting to the web
##' page listing the number of flavors, or you do not want to use caching.
##'
##' The options can be set:
##'
##' * by session, using, for instance,`options("foghorn.use_cache" = FALSE)`.
##' * permanently, by adding `options("foghorn.use_cache" = FALSE)` in your
##'   `.Rprofile`.
##' * for a specific call, using the `withr` package:
##'   `withr::with_options(foghorn.use_cache = FALSE,  ...)`.
##'
##' @param use_cache Should the value for the number of flavors be read to/
##'   written from the cache? (default: `TRUE`)
##' @param force_default Should the default value be used? (default: `FALSE`).
##'   When `TRUE`, the number of flavors is read from the Internet.
##' @param n_flavors What is the default number of flavors? (default: `14L`)
##' @return The number of CRAN check flavors (as an integer).
##' @export
##' @importFrom rlang is_logical is_integer is_lgl_na
##' @importFrom xml2 read_html
##' @importFrom rvest html_node html_table
n_cran_flavors <- function(use_cache = getOption("foghorn.use_cache", TRUE),
                           force_default = getOption(
                             "foghorn.force_default",
                             FALSE
                           ),
                           n_flavors = getOption("foghorn.n_flavors", 14L)) {
  stopifnot(rlang::is_logical(use_cache, 1) && !rlang::is_lgl_na(use_cache))
  stopifnot(rlang::is_logical(force_default, 1) && !rlang::is_lgl_na(use_cache))
  stopifnot(rlang::is_integer(n_flavors, 1))

  ## maximum number of CRAN check results that can be expected.
  ## As of 2021-05-31, 14 flavors listed
  ## https://cran.r-project.org/web/checks/check_flavors.html

  if (force_default) {
    return(n_flavors)
  }

  file_name <- "foghorn-n_cran_flavors.rds"
  cache_location <- file.path(tempdir(), file_name)

  if (file.exists(cache_location) && use_cache) {
    return(readRDS(cache_location))
  }

  check_flavors_url <- paste0(
    cran_url(),
    "/web/checks/check_flavors.html"
  )

  cran_flavors <- try(
    xml2::read_html(check_flavors_url),
    silent = TRUE
  )

  if (inherits(cran_flavors, "try-error")) {
    warning(
      "Can't connect to check number of CRAN flavors. ",
      "Using default value: ",
      n_flavors,
      ".",
      call. = FALSE
    )
    return(n_flavors)
  }

  cran_flvr_tbl <- rvest::html_table(
    rvest::html_node(cran_flavors, "table")
  )

  res <- nrow(cran_flvr_tbl)

  if (use_cache) {
    saveRDS(res, file = cache_location)
  }

  res
}
