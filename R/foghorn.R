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

##' @importFrom dplyr %>%
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
    res_checks <- cran_results(email, pkg, ...)
    what <- c("ERROR", "FAIL", "WARN", "NOTE", "has_other_issues")
    res <- lapply(what, function(x)
        get_pkg_with_results(res_checks, x, compact))
    mapply(function(type, pkgs, compact) {
        print_summary_cran(type, pkgs, compact)
    }, what, res, compact)
   invisible(res_checks)
}


