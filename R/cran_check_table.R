cran_checks_table <- function(parsed, ...) UseMethod("cran_checks_table")


##' @importFrom rvest html_table
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
            .res <- cran_checks_table.cran_checks_pkg(read_cran_web_from_pkg(pkg))
        } else
            .res <- tibble::as_tibble(tbl[[1]])
        add_cols(.res)
    })
    do.call("rbind", res)
}

##' @importFrom stats reshape xtabs
process_cran_table <- function(tbl) {
    ctbl <- xtabs(~ package + version + status, data = tbl)
    ctbl <- as.data.frame(ctbl, stringsAsFactors = FALSE)
    ctbl <- ctbl[ctbl$Freq > 0, ]
    ctbl <- stats::reshape(ctbl, idvar = c("package", "version"),
                           timevar = "status", direction = "wide",
                           v.name = "Freq")
    names(ctbl) <- gsub("Freq\\.", "", names(ctbl))
    names(ctbl) <- tolower(names(ctbl))
    ctbl <- tibble::as_tibble(ctbl)
    add_cols(ctbl)
}

cran_checks_table.cran_checks_pkg <- function(parsed, ...) {
    tbl <- get_cran_table(parsed, .id = "Package")
    process_cran_table(tbl)
}


cran_checks_table.crandb <- function(parsed, ...) {
    tbl <- parsed
    tbl$Status <- gsub("WARNING", "WARN", tbl$Status)
    process_cran_table(tbl)
}


cran_checks_table.api <- function(parsed, ...) {
    process_cran_table(parsed)
}
