cran_results_web <- function(email, pkg, max_requests, ...) {
  res <- NULL
  check_n_requests(email, pkg, max_requests = max_requests)
  if (!is.null(email)) {
    res_email <- read_cran_web_from_email(email, ...)
    res <- cran_checks_table(res_email)
    to_fix <- fix_before_pkg_web(unlist(all_packages(res_email)), max_requests = max_requests)
    res <- add_fix_before(res, to_fix)
    res <- add_other_issues(res, res_email)
  }
  if (!is.null(pkg)) {
    res_pkg <- read_cran_web_from_pkg(pkg, ...)
    tbl_pkg <- cran_checks_table(res_pkg)
    res_pkg <- add_other_issues(tbl_pkg, res_pkg)
    res_pkg <- add_fix_before(res_pkg, fix_before_pkg_web(pkg))
    res <- rbind(res, res_pkg)
  }
  res
}


cran_results_crandb <- function(email, pkg, ...) {
  res <- NULL
  if (!is.null(email)) {
    res_email <- read_crandb_from_email(email, ...)
    res <- cran_checks_table(res_email)
    res <- add_other_issues_crandb(res)
    res <- add_fix_before(res, fix_before_email_crandb(email))
  }
  if (!is.null(pkg)) {
    res_pkg <- read_crandb_from_pkg(pkg, ...)
    tbl_pkg <- cran_checks_table(res_pkg)
    res_pkg <- add_other_issues_crandb(tbl_pkg)
    res_pkg <- add_fix_before(res_pkg, fix_before_pkg_crandb(pkg))
    res <- rbind(res_pkg, res)
  }
  res
}


##' Make a table that summarizes the results of the CRAN checks for a
##' set of packages specified by a maintainer or by names.
##'
##'
##' Given the email address of a package maintainer, and/or a vector
##' of package names, returns a tibble that allows you to detect
##' potential issues with your packages on CRAN.
##'
##'
##' @title Table of the CRAN check results
##' @export
##' @param email email address for package maintainers (character
##'     vector)
##' @param pkg package names (character vector)
##' @param show columns of the data frame to show (all are shown by
##'     default)
##' @template src
##' @param max_requests maximum number of requests allowed to be performed.
##' @template dots
##' @template details
##' @return a data frame that tabulates the number of CRAN flavors
##'     that return errors, warnings, notes, or OK for the packages.
##' @examples
##'   \dontrun{
##'     cran_results(pkg="MASS")
##'   }
cran_results <- function(email = NULL, pkg = NULL,
                         show = c("error", "fail", "warn", "note", "ok"),
                         src = c("website", "crandb"),
                         max_requests = 20, ...) {
  show <- tolower(show)
  show <- match.arg(show, several.ok = TRUE)
  show <- c("package", show, "has_other_issues", "fix_before")

  src <- match.arg(src, c("website", "crandb"))

  if (is.null(email) && is.null(pkg)) {
    stop(
      "You need to provide at least one value for ", sQuote("email"),
      "or for ", sQuote("pkg")
    )
  }

  if (identical(src, "website")) {
    res <- cran_results_web(email, pkg, max_requests = max_requests, ...)
  } else if (identical(src, "crandb")) {
    res <- cran_results_crandb(email, pkg, ...)
  }

  if (nrow(res) < 1L) {
    stop(
      "Failed to get CRAN results, please report this issue: ",
      "https://github.com/fmichonneau/foghorn/issues/new/",
      call. = FALSE
    )
  }

  res <- res[!duplicated(res$package), ]
  res <- res[order(res$package), show]
  class(res) <- c("cran_results", class(res))
  res
}


##' Summary of the CRAN check results
##'
##' Given the email address of a package maintainer, and/or a vector
##' of package names, it displays at the console a summary of the
##' check results run on the CRAN flavors. This function is designed
##' to be included in your .Rprofile to be run (periodically) at start
##' up.
##'
##' @param email email address for package maintainers (character vector)
##' @param pkg package names (character vector)
##' @param compact if \code{TRUE}, all the packages with non-OK results are
##'     listed in a single line, otherwise they are listed on multiple lines.
##' @template print_ok
##' @template dots
##' @examples \dontrun{
##'    summary_cran_results(email = c("user1@company1.com", "user2@company2.com"))
##'    summary_cran_results(email = "user1@company1.com",
##'                         pkg = c("pkg1", "pkg2"))
##' }
##' @return Prints the packages that return errors, warnings, and
##'     notes on the CRAN flavors. The number in parenthesis after
##'     the name of the packages indicates the number of CRAN
##'     flavors that produce these results.
##' @importFrom cli col_red col_yellow col_blue style_bold col_cyan col_magenta symbol
##' @export
summary_cran_results <- function(email = NULL, pkg = NULL,
                                 compact = FALSE, print_ok = TRUE, ...) {
  res_checks <- cran_results(email, pkg, ...)
  summary(res_checks, compact = compact, print_ok = print_ok)
}

##' @param object an object created by \code{cran_results}
##' @export
##' @rdname summary_cran_results
summary.cran_results <- function(object, compact = FALSE, print_ok = TRUE, ...) {
  what <- c("ok", "error", "fail", "warn", "note", "has_other_issues")
  lapply(what, function(x) {
    get_pkg_with_results(object, x, compact, print_ok)
  })
  invisible(object)
}
