cran_checks_table <- function(parsed, ...) UseMethod("cran_checks_table")


##' @importFrom rvest html_table
##' @importFrom dplyr bind_rows
##' @importFrom tibble as_tibble
cran_checks_table.cran_checks_email <- function(parsed, ...) {
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
            cran_checks_table.cran_checks_pkg(read_cran_web_from_pkg(pkg))
        } else
           tibble::as_tibble(tbl[[1]])
    })
    dplyr::bind_rows(res, default_cran_checks, ...) %>%
        convert_nas()
}

##' @importFrom dplyr %>%
##' @importFrom dplyr count_ bind_rows ungroup
##' @importFrom tidyr spread
cran_checks_table.cran_checks_pkg <- function(parsed, ...) {
    tbl <- get_cran_table(parsed, .id = "Package")

    tbl %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup() %>%
        convert_nas()
}


cran_checks_table.crandb <- function(parsed, ...) {
    parsed$Status <- gsub("WARNING", "WARN", parsed$Status)
    parsed %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup() %>%
        convert_nas()
}


cran_checks_table.api <- function(parsed, ...) {
    parsed %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup() %>%
        convert_nas()
}
