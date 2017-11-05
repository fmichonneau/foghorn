url_pkg_res <- function(pkg) {
    paste0("https://cran.r-project.org/web/checks/check_results_", pkg, ".html")
}

url_email_res <- function(email) {
    email <- convert_email_to_cran_format(email)
    paste0("https://cran.r-project.org/web/checks/check_results_",
           email, ".html")
}

##' @importFrom rlang .data
summary_pkg_res <- function(res) {
    res$data$checks %>%
        purrr::map_df(function(x) {
                   list(Flavor = x$flavor, Version = x$version,
                        tinstall = x$tinstall, tcheck = x$tcheck, ttotal = x$ttotal,
                        Status = x$status, check_url = x$check_url %||% NA_character_)
               })  %>%
        dplyr::bind_cols(Package = rep(res$data$package, nrow(rlang::.data)))
}

summary_maintainer_res <- function(res) {
     ## TODO
}

##' @importFrom xml2 read_html
##' @importFrom curl has_internet
read_cran_web <- function(x) {
    if (!curl::has_internet()) {
        stop("No internet connection detected", call. = FALSE)
    }
    .res <- try(xml2::read_html(x), silent = TRUE)
    if (inherits(.res, "try-error")) {
        ## is there a cleaner way to do this ???
        ss <- showConnections(all = TRUE)
        cc <- as.numeric(rownames(ss)[ss[, 1] == x])
        if (length(cc) > 0) on.exit(close(getConnection(cc)))
        return(NA)
    }
    ## not sure whether we need to close connections ... ?
    return(.res)
}

read_cran_web_from_email <- function(email) {
    url <- url_email_res(email)
    res <- lapply(url, read_cran_web)
    if (length(bad <- which(is.na(res))) > 0) {
        stop("Invalid email address(es): ", email[bad], call. = FALSE)
    }
    class(res) <- c("cran_checks_email", class(res))
    res
}

read_cran_web_from_pkg <- function(pkg) {
    url <- url_pkg_res(pkg)
    res <- lapply(url, read_cran_web)
    if (length(bad <- which(is.na(res))) > 0) {
        stop("Invalid package name(s): ", pkg[bad], call. = FALSE)
    }
    names(res) <- pkg
    class(res) <- c("cran_checks_pkg", class(res))
    res
}


##' @importFrom tibble tibble
default_cran_checks <- tibble::tibble(
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0),
    FAIL = integer(0))


get_cran_table <- function(parsed, ...) {
    res <- lapply(parsed, function(x) {
        tbl <- rvest::html_table(x)
        tbl <- tbl[[1]]
        tbl$Version <- as.character(tbl$Version)
        tbl
    })
    names(res) <- names(parsed)
    dplyr::bind_rows(res, ...) %>%
        dplyr::bind_rows(default_cran_checks)
}


all_packages <- function(parsed, ...) UseMethod("all_packages")

all_packages_by_email <- function(x) {
    xml2::xml_find_all(x, ".//h3/@id") %>%
        xml2::xml_text()
}

all_packages.cran_checks_email <- function(parsed, ...) {
    lapply(parsed, all_packages_by_email)
}

all_packages.cran_checks_pkg <- function(parsed, ...) {
    lapply(parsed, function(x) {
        res <- xml2::xml_find_all(x, ".//h2/a/text()") %>%
            xml2::xml_text()
        gsub("\\s", "", res)
    })
}

##' @importFrom dplyr `%>%`-
##' @importFrom tibble tibble
has_other_issues <- function(parsed, ...) {
    pkg <- all_packages(parsed)

    res <- lapply(pkg, function(x) {
        tibble::tibble(`Package` = x,
                       `has_other_issues` = rep(FALSE, length(x)))
    })

    res <- dplyr::bind_rows(res)
    pkg_with_issue <- lapply(parsed, function(x) {
        all_urls <- xml2::xml_find_all(x, ".//h3//child::a[@href]//@href") %>%
            xml2::xml_text()
        with_issue <- grep("check_issue_kinds", all_urls, value = TRUE)
        pkg_with_issue <- unique(basename(with_issue))
        if (length(pkg_with_issue) ==  0) return(NULL)
        TRUE
    })
    pkg_with_issue <- unlist(pkg_with_issue)
    res[["has_other_issues"]][match(names(pkg_with_issue), res$Package)] <- TRUE
    res
}

##' @importFrom dplyr left_join
add_other_issues <- function(tbl, parsed, ...) {
    other_issues <- has_other_issues(parsed)
    dplyr::left_join(tbl, other_issues, by = "Package")
}


table_cran_checks <- function(parsed, ...) UseMethod("table_cran_checks")


##' @importFrom rvest html_table
##' @importFrom dplyr bind_rows
##' @importFrom tibble as_tibble
table_cran_checks.cran_checks_email <- function(parsed, ...) {
    res <- lapply(parsed, function(x) {
        tbl <- rvest::html_table(x)
        if (length(tbl) < 1) {
            ## If there is no table on the page, the maintainer has
            ## authored a single package, let's look for it:
            pkg <- all_packages_by_email(x)
            if (length(pkg) > 1)
                stop("Please file an issue on GitHub ",
                     "indicating the name of your package")
            ## then, we can call the other method to parse the results
            ## of that pacakge
            table_cran_checks.cran_checks_pkg(read_cran_web_from_pkg(pkg))
        } else
           tibble::as_tibble(tbl[[1]])
    })
    dplyr::bind_rows(res, default_cran_checks, ...) %>%
        convert_nas()
}

##' @importFrom magrittr %>%
##' @importFrom dplyr count_ bind_rows ungroup
##' @importFrom tidyr spread
table_cran_checks.cran_checks_pkg <- function(parsed, ...) {
    tbl <- get_cran_table(parsed, .id = "Package")

    tbl %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup() %>%
        convert_nas()
}

table_cran_checks.api <- function(parsed, ...) {
    parsed %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup() %>%
        convert_nas()
}





get_pkg_with_results <- function(tbl_pkg, what, compact = FALSE, ...) {
    what <- match.arg(what, names(tbl_pkg)[-1])
    if (what %in% c("has_other_issues"))
        show_n <- FALSE
    else show_n <- TRUE
    if (sum(tbl_pkg[[what]],  na.rm = TRUE) > 0) {
        n <- tbl_pkg[[what]][tbl_pkg[[what]] > 0]
        if (show_n) {
            n <- paste0(" (", n, ")")
        } else
            n <- character(0)
        if (compact) {
            sptr <- c("", ", ")
        } else
            sptr <- c("  - ", "\n")
        paste0(sptr[1], tbl_pkg$Package[!is.na(tbl_pkg[[what]]) &
                                        tbl_pkg[[what]] > 0],
               n, collapse = sptr[2])
    }
}


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

print_summary_cran <- function(type = c("ERROR", "FAIL", "WARN",
                                        "NOTE", "has_other_issues"),
                               pkgs, compact) {
    if (is.null(pkgs))
        return(NULL)

    type <- match.arg(type)
    if (compact) {
        nl <- character(0)
    } else
        nl <- "\n"

    if (grepl(",|\\n", pkgs))
        pkg_string <- "Packages"
    else
        pkg_string <- "Package"

    msg <- paste(" ", pkg_string, "with", foghorn_components[[type]]$word,
                 "on CRAN: ")
    message(foghorn_components[[type]]$color(
        paste0(foghorn_components[[type]]$symbol,
               msg, nl,
               crayon::bold(pkgs))
    ))
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
    res_checks <- check_cran_results(email, pkg, ...)
    what <- c("ERROR", "FAIL", "WARN", "NOTE", "has_other_issues")
    res <- lapply(what, function(x)
        get_pkg_with_results(res_checks, x, compact))
    mapply(function(type, pkgs, compact) {
        print_summary_cran(type, pkgs, compact)
    }, what, res, compact)
   invisible(res_checks)
}


##' Visit the page in your web browser for a given package or a
##' maintainer's email address
##'
##' @title Visit the CRAN check results page
##' @param pkg name of the package to check the results for
##' @param email email address of the package maintainer
##' @return The URL from the CRAN check results page invisibly
##' @export
##' @importFrom utils browseURL
visit_cran_check <- function(pkg = NULL, email = NULL) {
    if (is.null(pkg) && is.null(email)) {
        stop("A package name or an email address needs to be specified",
             call. = FALSE)
    }
    if (!is.null(pkg) && !is.null(email)) {
        stop("Specify only one package or one email address", call. = FALSE)
    }
    if (!is.null(pkg)) {
        if (is.character(pkg) && length(pkg) == 1)
            url <- url_pkg_res(pkg)
        else
            stop(sQuote("pkg"), " must be a string")
    } else if (!is.null(email)) {
        if (is.character(email) && length(email) == 1)
            url <- url_email_res(email)
        else
            stop(sQuote("email"), " must be a string")
    }
    if (interactive())
        utils::browseURL(url)
    else
        warning("This function is only available in interactive mode.")
    invisible(url)
}
