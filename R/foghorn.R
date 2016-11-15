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
    email_test <- sapply(email, function(x)
        if (!grepl("\\@", x)) {
            stop("Malformed email address: ", sQuote(email), call. = FALSE)
        })
    email <- gsub("\\@", "_at_", email)
    ##  "all characters different from letters, digits, hyphens,
    ##  underscores, colons, and periods replaced by underscores ..."
    email <- gsub("[^[:alnum:]_:.-]","_",email)
    url <- paste0("https://cran.r-project.org/web/checks/check_results_",
                  email, ".html")
    res <- lapply(url, parse_cran)
    if (length(bad <- which(is.na(res)))>0) {
        stop("Invalid email address(es): ", email[bad], call. = FALSE)
    }
    class(res) <- c("cran_checks_email", class(res))
    res
}

parse_cran_checks_pkg <- function(pkg) {
    url <- url_pkg_res(pkg)
    res <- lapply(url, parse_cran)
    if (length(bad <- which(is.na(res)))>0) {
        stop("Invalid package name(s): ", pkg[bad], call. = FALSE)
    }
    names(res) <- pkg
    class(res) <- c("cran_checks_pkg", class(res))
    res
}


##' @importFrom tibble data_frame
default_cran_checks <- tibble::data_frame(
    NOTE = integer(0),
    OK = integer(0),
    WARN = integer(0),
    ERROR = integer(0))


get_cran_table <- function(parsed, ...) {
    res <- lapply(parsed, function(x) {
        tbl <- rvest::html_table(x)
        tbl[[1]]
    })
    dplyr::bind_rows(res, default_cran_checks, ...)
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

##' @importFrom tibble data_frame
has_memtest <- function(parsed, ...) {
    pkg <- all_packages(parsed)

    res <- lapply(pkg, function(x) {
        tibble::data_frame(`Package` = x,
                          `has_memtest_notes` = rep(FALSE, length(x))
                          )
    })
    res <- dplyr::bind_rows(res)
    pkg_with_mem <- lapply(parsed, function(x) {
        all_urls <- xml2::xml_text(xml2::xml_find_all(x, ".//p//child::a[@href]//@href"))
        with_mem <- grep("memtest", all_urls, value = TRUE)
        with_mem <- grep("[^Rout]$", with_mem, value = TRUE)
        pkg_with_mem <- unique(basename(with_mem))
        pkg_with_mem
    })
    pkg_with_mem <- unlist(pkg_with_mem)
    res[["has_memtest_notes"]][match(pkg_with_mem, res$Package)] <- TRUE
    res
}

add_memtest <- function(tbl, parsed, ...) {
    memtest <- has_memtest(parsed)
    dplyr::left_join(tbl, memtest, by = "Package")
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
                stop("Please file an issue on GitHub indicating the name of your package")
            ## then, we can call the other method to parse the results of that pacakge
            table_cran_checks.cran_checks_pkg(parse_cran_checks_pkg(pkg))
        } else
           tibble::as_tibble(tbl[[1]])
    })
    dplyr::bind_rows(res, default_cran_checks, ...)
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
        dplyr::ungroup()
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
##' @title Table of CRAN check results
##' @importFrom dplyr distinct_
##' @export
##' @param email email address for package maintainers (character
##'     vector)
##' @param package package names (character vector)
##' @param show columns of the data frame to show
##' @return a data frame that tabulates the number of CRAN platforms
##'     that return errors, warnings, notes, or OK for the packages.
cran_check_results <- function(email = NULL, package = NULL,
                               show = c("error", "warn", "note", "ok")) {
    show <- match.arg(show, several.ok = TRUE)
    show <- c("Package", toupper(show), "has_memtest_notes")
    res <- NULL
    if (is.null(email) && is.null(package))
        stop("You need to provide at least one value for ", sQuote("email"),
             "or for ", sQuote("package"))
    if (!is.null(email)) {
        res_email <- parse_cran_checks_email(email)
        res <- table_cran_checks(res_email)
        res <- add_memtest(res, res_email)
    }
    if (!is.null(package)) {
        res_pkg <- parse_cran_checks_pkg(package)
        tbl_pkg <- table_cran_checks(res_pkg)
        res <- add_memtest(tbl_pkg, res_pkg) %>%
            dplyr::bind_rows(res)
    }
    res <- dplyr::distinct_(res, "Package", .keep_all = TRUE)
    res[, show]
}



summary_functional <- function(what, show_n = TRUE) {
    function(tbl_pkg, ...) {
        if (sum(tbl_pkg[[what]],  na.rm = TRUE) > 0) {
            n <- tbl_pkg[[what]][!is.na(tbl_pkg[[what]])]
            if (show_n) {
                n <- paste0(" (", n, ")")
            } else
            paste0(tbl_pkg$Package[!is.na(tbl_pkg[[what]]) & tbl_pkg[[what]] > 0],
                n <- character(0)
                  n,
                  collapse = ", ")
        }
    }
}

summary_error <- summary_functional("ERROR", TRUE)
summary_note <- summary_functional("NOTE", TRUE)
summary_warning <- summary_functional("WARN", TRUE)
summary_memtest <- summary_functional("has_memtest_notes", FALSE)

##' Summary of the CRAN check results
##'
##' Given the email address of a package maintainer, and/or a vector
##' of package names, it displays at the console a summary of the
##' check results run on the CRAN platforms. This function is designed
##' to be included in your .Rprofile to be run (periodically) at start
##' up.
##'
##' @importFrom crayon red yellow blue bold cyan
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
    res_checks <- cran_check_results(email, package)
    pkg_err <- summary_error(res_checks)
    pkg_wrn <- summary_warning(res_checks)
    pkg_note <- summary_note(res_checks)
    pkg_memtest <- summary_memtest(res_checks)
    if (!is.null(pkg_err))
        message(crayon::red(paste(clisymbols::symbol$cross,
                                  "Package(s) with errors on CRAN:", crayon::bold(pkg_err))))
    if (!is.null(pkg_wrn))
        message(crayon::yellow(paste(clisymbols::symbol$warning,
                                     "Package(s) with warnings on CRAN:", crayon::bold(pkg_wrn))))
    if (!is.null(pkg_note))
        message(crayon::blue(paste(clisymbols::symbol$star,
                                   "Package(s) with notes on CRAN:", crayon::bold(pkg_note))))
    if (!is.null(pkg_memtest))
        message(crayon::cyan(paste(clisymbols::symbol$circle_filled,
                                   "Packages(s) with memtest notes:", crayon::bold(pkg_memtest))))
    invisible(res_checks)
}

url_pkg_res <- function(pkg) {
    paste0("https://cran.r-project.org/web/checks/check_results_", pkg, ".html")
}


##' Visit the page in your web browser for a given package.
##'
##' @title Visit the CRAN check results page for a package
##' @param pkg name of the package to check the results for
##' @return The URL from the CRAN check results page invisibly
##' @author Francois Michonneau
##' @export
##' @importFrom utils browseURL
visit_cran_check <- function(pkg) {
    url <- url_pkg_res(pkg)
    if (interactive())
        utils::browseURL(url)
    else
        warning("This function is only available in interactive mode.")
    invisible(url)
}

parse_cran_results <- function(pkg, what = c("error", "warning", "note"), ...) {
    what <- match.arg(what)
    parsed <- parse_cran_checks_pkg(pkg)
    mem_test <- has_memtest(parsed)

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
    res <- dplyr::bind_rows(all_p, .id = "Package")
    attr(res, "memtest") <- mem_test
    res
}

##' @importFrom crayon red yellow blue
##' @importFrom clisymbols symbol
format_cran <- function(type, string) {
    col_tbl <- list(
        "ERROR" = function(x) crayon::red(paste(clisymbols::symbol$cross, x)),
        "WARN" = function(x) crayon::yellow(paste(clisymbols::symbol$warning, x)),
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
##' @importFrom crayon bold silver
summary_cran_results <- function(pkg, verbose = TRUE) {
    res <- parse_cran_results(pkg)
    if (nrow(res) < 1) {
        message("All clear for ", paste(pkg, collapse = ", "))
        return(invisible(NULL))
    }
    apply(attr(res, "memtest"), 1, function(x) {
        if (x[2])
            cat(crayon::cyan(paste(clisymbols::symbol$circle_filled, crayon::bold(x[1]), "has memtest notes")), "\n")
    })
    apply(res, 1, function(x)  {
        if (verbose)
            msg <- crayon::silver(x[5])
        else
            msg <- character(0)
        cat(## Type of CRAN message
            format_cran(x[2], paste0(crayon::bold(paste(x[1], "-", x[2])), ": ", x[3])), "\n",
            ## Flavors concerned
            render_flavors(x[4]), "\n",
            ## Optionally the log output
            msg, "\n\n", sep = "")
    })
    invisible(NULL)
}
