##' @importFrom xml2 read_html
parse_cran <- function(x) {
    .res <- try(xml2::read_html(x), silent = TRUE)
    if (inherits(.res, "try-error")) {
        ## is there a cleaner way to do this ???
        ss <- showConnections(all=TRUE)
        cc <- as.numeric(rownames(ss)[ss[,1]==x])
        if (length(cc)>0) on.exit(close(getConnection(cc)))
        return(NA)
    }
    ## not sure whether we need to close connections ... ?
    return(.res)
}

parse_cran_checks_email <- function(email) {
    if (!grepl("\\@", email))
        stop("Malformed email address: ", sQuote(email), call. = FALSE)
    email <- gsub("\\@", "_at_", email)
    ##  "all characters different from letters, digits, hyphens, underscores, colons, and periods replaced by underscores ..."
    email <- gsub("[^[:alnum:]_:.-]","_",email)
    url <- paste0("https://cran.r-project.org/web/checks/check_results_",
           email, ".html")
    res <- lapply(url, parse_cran)
    if (length(bad <- which(is.na(res)))>0) {
        stop("Invalid email address(es): ", email[bad], call. = FALSE)
    }
    class(res) <- "cran_checks_email"
    res
}

parse_cran_checks_pkg <- function(pkg) {
    url <- url_pkg_res(pkg)
    res <- lapply(url, parse_cran)
    names(res) <- pkg
    class(res) <- "cran_checks_pkg"
    res
}

table_cran_checks <- function(parsed, ...) UseMethod("table_cran_checks")

default_cran_checks <- data.frame(
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0),
    stringsAsFactors = FALSE)

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
##' @param email email address for package maintainers (character
##'     vector)
##' @param package package names (character vector)
##' @param show columns of the data frame to show
##' @return a data frame that tabulates the number of CRAN platforms
##'     that return errors, warnings, notes, or OK for the packages.
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
##' of package names, it displays at the console a summary of the
##' check results run on the CRAN platforms. This function is designed
##' to be included in your .Rprofile to be run (periodically) at start
##' up.
##'
##' This package tries to guess your email address by using the
##' \code{\link[whoami]{email_address}} function. You can specify email
##' addresses manually.
##'
##' @importFrom crayon red yellow blue bold
##' @importFrom clisymbols symbol
##' @export
##' @param email email address for package maintainers (character
##'     vector)
##' @param package package names (character vector)
##' @examples \dontrun{ summary_cran_checks(email =
##'     c("user1@company1.com", "user2@company2.com"))
##'     summary_cran_checks(email = "user1@company1.com", package =
##'     c("pkg1", "pkg2")) }
##' @return Prints the packages that return errors, warnings, and
##'     notes on the CRAN platforms. The number in parenthesis after
##'     the name of the packages indicates the number of CRAN
##'     platforms that produce these results.
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
    invisible(res_checks)
}

url_pkg_res <- function(pkg) {
    paste0("https://cran.r-project.org/web/checks/check_results_", pkg, ".html")
}


##' Visit the page in your web browser for a given package.
##'
##' @title Visit the CRAN check results page for a package
##' @param pkg name of the package to check the results for
##' @return \code{TRUE} invisibly
##' @author Francois Michonneau
##' @export
##' @importFrom utils browseURL
visit_cran_check <- function(pkg) {
    url <- url_pkg_res(pkg)
    utils::browseURL(url)
    invisible(TRUE)
}

parse_cran_results <- function(pkg, what = c("error", "warning", "note"), ...) {
    what <- match.arg(what)
    parsed <- parse_cran_checks_pkg(pkg)

    all_p <- lapply(parsed, function(x) {
        p <- xml2::xml_find_all(x, ".//p")
        p <- strsplit(xml2::xml_text(x), "\n")
        p <- unlist(p)
        p <- p[nzchar(p)]
        p <- gsub(intToUtf8(160), " ", p)
        chk_idx <- grep("^Check:", p)
        res_idx <- grep("^Result:", p)
        flv_idx <- grep("^Flavors?:", p)
        if (!identical(length(chk_idx), length(res_idx)) &&
            !identical(length(chk_idx), length(flv_idx)))
            stop("File an issue on Github indicating the name of your package.")
        msg <- mapply(function(c, r, f) {
            data.frame(
                result = gsub("^Result: ", "", p[r]),
                check = gsub("^Check: ", "", p[c]),
                flavors = gsub("^Flavors?: ", "", p[f]),
                message = paste(p[(r+1):(f-1)], collapse = "\n"),
                stringsAsFactors = FALSE
            )
        }, chk_idx, res_idx, flv_idx, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        dplyr::bind_rows(msg)
    })
    names(all_p) <- pkg
    dplyr::bind_rows(all_p, .id = "Package")
}

##' @importFrom crayon red yellow blue
##' @importFrom clisymbols symbol
format_cran <- function(type, string) {
    col_tbl <- list(
        "ERROR" = function(x) crayon::red(paste(clisymbols::symbol$cross, x)),
        "WARN" = function(x) crayon::yellow(paste(clisymbols::symbol$circle_filled, x)),
        "NOTE" = function(x) crayon::blue(paste(clisymbols::symbol$star, x))
    )
    col_tbl[[type]](string)
}


##' @importFrom clisymbols symbol
render_flavors <- function(x) {
    ## transform the comma separated list of platform flavors into
    ## unordered list
    res <- unlist(strsplit(x, ", "))
    paste("  ", clisymbols::symbol$pointer, res, "\n")
}


##' Given a package name published on CRAN, print the outcomes of
##' checks that return notes, warnings or errors, and optionally print
##' the messages.
##'
##' @title Summarize CRAN results for a package
##' @param pkg name of the package on CRAN
##' @param verbose Should the messages of the \dQuote{Check Details} be printed? (logical)
##' @return \code{NULL}, used for its side effect of printing the CRAN messages
##' @export
##' @importFrom crayon bold
summary_cran_results <- function(pkg, verbose = TRUE) {
    res <- parse_cran_results(pkg)
    if (nrow(res) < 1) {
        message("All clear for ", paste(pkg, collapse = ", "))
        return(invisible(NULL))
    }
    apply(res, 1, function(x)  {
        if (verbose)
            msg <- crayon::silver(x[5])
        else
            msg <- ""
        cat(## Type of CRAN message
            format_cran(x[2], paste0(crayon::bold(paste(x[1], "-", x[2])), ": ", x[3])), "\n",
            ## Flavors concerned
            render_flavors(x[4]), "\n",
            ## Optionally the log output
            msg, "\n\n", sep = "")
    })
    invisible(NULL)
}
