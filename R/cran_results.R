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
##' @importFrom dplyr distinct_
##' @export
##' @param email email address for package maintainers (character
##'     vector)
##' @param pkg package names (character vector)
##' @param show columns of the data frame to show (all are shown by
##'     default)
##' @template src
##' @template dots
##' @template details
##' @return a data frame that tabulates the number of CRAN platforms
##'     that return errors, warnings, notes, or OK for the packages.
##' @examples
##'   if (curl::has_internet()) {
##'     cran_results(pkg="MASS")
##'   }
cran_results <- function(email = NULL, pkg = NULL,
                         show = c("error", "fail", "warn", "note", "ok"),
                         src = c("website", "crandb"), ...) {
    show <- tolower(show)
    show <- match.arg(show, several.ok = TRUE)
    show <- c("Package", toupper(show), "has_other_issues")
    res <- NULL

    src <- match.arg(src, c("website", "crandb"))

    if (is.null(email) && is.null(pkg))
        stop("You need to provide at least one value for ", sQuote("email"),
             "or for ", sQuote("pkg"))

    if (identical(src, "website")) {
        if (!is.null(email)) {
            res_email <- read_cran_web_from_email(email)
            res <- cran_checks_table(res_email)
            res <- add_other_issues(res, res_email)
        }
        if (!is.null(pkg)) {
            res_pkg <- read_cran_web_from_pkg(pkg)
            tbl_pkg <- cran_checks_table(res_pkg)
            res_pkg <- add_other_issues(tbl_pkg, res_pkg)
            res <- rbind(res, res_pkg)
        }
    } else if (identical(src, "crandb")) {
        if (!is.null(email)) {
            res_email <- crandb_pkg_info_email(email, ...)
            res <- cran_checks_table(res_email)
            res <- add_other_issues_crandb(res)
        }
        if (!is.null(pkg)) {
            res_pkg <- crandb_pkg_info_pkg(pkg, ...)
            tbl_pkg <- cran_checks_table(res_pkg)
            res_pkg <- add_other_issues_crandb(tbl_pkg)
            rbind(res_pkg, res)
        }

    }
    res <- res[!duplicated(res$Package), ]
    res <- res[, show]
    class(res) <- c("cran_results", class(res))
    res
}


##' Summary of the CRAN check results
##'
##' Given the email address of a package maintainer, and/or a vector
##' of package names, it displays at the console a summary of the
##' check results run on the CRAN platforms. This function is designed
##' to be included in your .Rprofile to be run (periodically) at start
##' up.
##'
##' @param email email address for package maintainers (character
##'     vector)
##' @param pkg package names (character vector)
##' @param compact if \code{TRUE}, all the packages with non-OK
##'     results are listed in a single line, otherwise they are listed
##'     on multiple lines.
##' @template dots
##' @examples \dontrun{
##'    summary_cran_results(email = c("user1@company1.com", "user2@company2.com"))
##'    summary_cran_results(email = "user1@company1.com",
##'                         pkg = c("pkg1", "pkg2"))
##' }
##' @return Prints the packages that return errors, warnings, and
##'     notes on the CRAN platforms. The number in parenthesis after
##'     the name of the packages indicates the number of CRAN
##'     platforms that produce these results.
##' @importFrom crayon red yellow blue bold cyan magenta
##' @importFrom clisymbols symbol
##' @export
summary_cran_results <- function(email = NULL, pkg = NULL,
                                compact = FALSE, ...) {
    res_checks <- cran_results(email, pkg, ...)
    summary(res_checks, compact = compact)
}

##' @param object an object created by \code{cran_results}
##' @export
##' @rdname summary_cran_results
summary.cran_results <- function(object, compact = FALSE, ...) {
  what <- c("ERROR", "FAIL", "WARN", "NOTE", "has_other_issues")
    res <- lapply(what, function(x)
        get_pkg_with_results(object, x, compact))
    mapply(function(type, pkgs, compact) {
        print_summary_cran(type, pkgs, compact)
    }, what, res, compact)
   invisible(object)
}
