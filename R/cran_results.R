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
            res <- table_cran_checks(res_email)
            res <- add_other_issues(res, res_email)
        }
        if (!is.null(pkg)) {
            res_pkg <- read_cran_web_from_pkg(pkg)
            tbl_pkg <- table_cran_checks(res_pkg)
            res <- add_other_issues(tbl_pkg, res_pkg) %>%
                dplyr::bind_rows(res)
        }
    } else if (identical(src, "crandb")) {
        if (!is.null(email)) {
            res_email <- crandb_pkg_info_email(email, ...)
            res <- table_cran_checks(res_email)
            res <- add_other_issues_crandb(res)
        }
        if (!is.null(pkg)) {
            res_pkg <- crandb_pkg_info_pkg(pkg, ...)
            tbl_pkg <- table_cran_checks(res_pkg)
            res <- add_other_issues_crandb(tbl_pkg) %>%
                dplyr::bind_rows(res)
        }

    }
    res <- dplyr::distinct_(res, "Package", .keep_all = TRUE)
    res[, show]
}
