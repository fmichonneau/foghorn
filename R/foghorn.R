##' @importFrom xml2 read_html
parse_cran <- function(x) {
    .res <- try(xml2::read_html(x), silent = TRUE)
    if (inherits(.res, "try-error")) {
        on.exit(close(x))
        stop("Invalid email address or package: ", .res, call. = FALSE)
    }
    .res
}

parse_cran_checks_email <- function(email) {
    if (!grepl("\\@", email))
        stop("Malformed email address: ", sQuote(email), call. = FALSE)
    email <- gsub("\\@", "_at_", email)
    url <- paste0("https://cran.r-project.org/web/checks/check_results_",
           email, ".html")
    res <- lapply(url, parse_cran)
    class(res) <- "cran_checks_email"
    res
}

parse_cran_checks_pkg <- function(pkg) {
    url <- paste0("https://cran.r-project.org/web/checks/check_results_",
                  pkg, ".html")
    res <- lapply(url, parse_cran)
    names(res) <- pkg
    class(res) <- "cran_checks_pkg"
    res
}

table_cran_checks <- function(parsed, ...) UseMethod("table_cran_checks")

##' @importFrom tibble data_frame
default_cran_checks <- tibble::data_frame(
    #Package = integer(0),
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0))

##' @importFrom rvest html_table
##' @importFrom dplyr bind_rows
table_cran_checks.cran_checks_email <- function(parsed, ...) {
    res <- lapply(parsed, function(x)
        rvest::html_table(x)[[1]])
    dplyr::bind_rows(res,
                     default_cran_checks,
                     ...)
}

##' @importFrom magrittr %>%
##' @importFrom dplyr count_ bind_rows ungroup
##' @importFrom tidyr spread
table_cran_checks.cran_checks_pkg <- function(parsed, ...) {
    tbl <- table_cran_checks.cran_checks_email(parsed, .id = "Package")
    res <- tbl %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup()
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
##' This package tries to guess your email address by using the
##' \code{\link[whoami]{email_address}} function. You can specify email
##' addresses manually.
##'
##'
##' @title Table of CRAN check results
##' @importFrom whoami email_address
##' @importFrom dplyr distinct_
##' @export
##' @param email email address for package maintainers (character vector)
##' @param package package names (character vector)
##' @param show columns of the data frame to show
cran_check_results <- function(email = whoami::email_address(), package = NULL,
                               show = c("error", "warn", "note", "ok")) {
    show <- match.arg(show, several.ok = TRUE)
    show <- c("Package", toupper(show))
    res <- NULL
    if (is.null(email) && is.null(package))
        stop("You need to provide at least one value for ", sQuote("email"),
             "or for ", sQuote("package"))
    if (!is.null(email)) {
        res_email <- parse_cran_checks_email(email)
        res <- table_cran_checks(res_email)
    }
    if (!is.null(package)) {
        res_pkg <- parse_cran_checks_pkg(package)
        res <- table_cran_checks(res_pkg) %>%
            dplyr::bind_rows(res)
    }
    res <- dplyr::distinct_(res, "Package", .keep_all = TRUE)
    res[, show]
}



summary_functional <- function(what) {
    function(tbl_pkg, ...) {
        if (sum(tbl_pkg[[what]],  na.rm = TRUE) > 0) {
            n <- tbl_pkg[[what]][!is.na(tbl_pkg[[what]])]
            paste(tbl_pkg$Package[!is.na(tbl_pkg[[what]]) & tbl_pkg[[what]] > 0],
                  paste0("(", n, ")"),
                  collapse = ", ")
        }
    }
}

summary_error <- summary_functional("ERROR")
summary_note <- summary_functional("NOTE")
summary_warning <- summary_functional("WARN")


##' Summary of the CRAN check results
##'
##' Given the email address of a package maintainer, and/or a vector
##' of package names, it displays at the console a summary of the CRAN
##' check results. This function is designed to be included in your
##' .Rprofile to be run (periodically) at start up.
##'
##' This package tries to guess your email address by using the
##' \code{\link[whoami]{email_address}} function. You can specify email
##' addresses manually.
##'
##' @importFrom crayon red yellow blue bold
##' @importFrom clisymbols symbol
##' @export
##' @param email email address for package maintainers (character vector)
##' @param package package names (character vector)
##' @examples \dontrun{
##'  summary_cran_checks(email = c("user1@company1.com", "user2@company2.com"))
##'  summary_cran_checks(email = "user1@company1.com", package = c("pkg1", "pkg2"))
##' }
summary_cran_checks <- function(email = whoami::email_address(), package = NULL) {
    res_checks <- cran_check_results(email, package)
    pkg_err <- summary_error(res_checks)
    pkg_wrn <- summary_warning(res_checks)
    pkg_note <- summary_note(res_checks)
    if (!is.null(pkg_err))
        message(crayon::red(paste(clisymbols::symbol$cross,
                                  "Package(s) with errors on CRAN:", crayon::bold(pkg_err))))
    if (!is.null(pkg_wrn))
        message(crayon::yellow(paste(clisymbols::symbol$circle_filled,
                                     "Package(s) with warnings on CRAN:", crayon::bold(pkg_wrn))))
    if (!is.null(pkg_note))
        message(crayon::blue(paste(clisymbols::symbol$star,
                                   "Package(s) with notes on CRAN:", crayon::bold(pkg_note))))
    invisible(list(
        with_error = pkg_err,
        with_warning = pkg_wrn,
        with_note = pkg_note
    ))
}


## view_warning <- function(parsed, ...) {
##     all_p <- xml_find_all(parsed, ".//p")
##     which_warn <- grep("Current CRAN status:\\nWARN", all_p)
##     res <- xml_text(all_p[which_warn + 1])
##     res <- gsub("\\n{2,}", "\n", res)
##     cat(res)
## }
