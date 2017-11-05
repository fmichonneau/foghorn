##' @importFrom httr GET write_disk status_code
fetch_cran_rds_file <- function(file = c("details", "results", "flavors", "issues"),
                                dest = tempdir(), protocol = c("https", "http"),
                                overwrite = FALSE, file_prefix = NULL, ...) {
    file <- match.arg(file)
    protocol <- match.arg(protocol)
    is_ftp <- if (identical(protocol, "ftp")) "pub/R/" else character(0)
    file <- paste0("check_", file, ".rds")
    dest_file <- file.path(dest, paste0(file_prefix, file))
    if (! (file.exists(dest_file) &&
           file.size(dest_file) > 0) ||
        overwrite) {
        cran_url <- paste0(protocol, "://cran.r-project.org/", is_ftp, "web/checks/", file)
        d_status <- httr::GET(url = cran_url,
                              httr::write_disk(dest_file, overwrite = overwrite), ...)
        if (!identical(httr::status_code(d_status), 200L))
            stop("Can't get ", cran_url, " (status code: ", httr::status_code(d_status), ")", call. = FALSE)
    }
    invisible(dest_file)
}

check_cran_rds_file <- function(file, return_logical = FALSE) {
    if (!file.exists(file))
        stop(file, " can't be found...", call. = FALSE)
    if (file.size(file) < 100)
        stop(file, " is corrupted. Delete it and retry.")
    res <- try(readRDS(file = file), silent = TRUE)
    if (inherits(res, "try-error"))
        stop(file, " can't be read. Delete it and retry.")
    else if (return_logical)
        return(TRUE)
    else res
}

read_cran_rds_file <- function(file) {
    res <- check_cran_rds_file(file)
    class(res) <- c("cran_db", class(res))
    res
}

## file can be a valid RDS file downloaded manually (or using a custom
## fetch_cran_rds_file() call), or the type of file that one wants to
## download from CRAN servers (e.g., "results", "details", "issues", ...)
get_cran_rds_file <- function(file, ...) {
    if (grepl("\\.rds$", file, ignore.case = TRUE) &&
        file.exists(file)) {
        f <- file
    } else {
        f <- fetch_cran_rds_file(file, ...)
    }
    read_cran_rds_file(f)
}

crandb_pkg_info_email <- function(email, file = "results", ...) {
    crandb <- get_cran_rds_file(file = file, ...)
    maintainer <- tolower(crandb$Maintainer)
    idx <- lapply(tolower(email), function(x)
        grepl(paste0("<", x, ">"), maintainer, fixed = TRUE))
    idx <- Reduce("+", idx)

    res <- crandb[as.logical(idx), ]
    class(res) <- c("crandb", class(res))
    res
}

crandb_pkg_info_pkg <- function(pkg, file = "results", ...) {
    crandb <- get_cran_rds_file(file = file, ...)
    res <- crandb[crandb[["Package"]] %in% pkg, ]
    class(res) <- c("crandb", class(res))
    res
}



add_other_issues_crandb <- function(tbl, ...) {
    issues <- get_cran_rds_file("issues", ...)
    res <- vapply(tbl[["Package"]], function(x) {
        pkg_issues <- issues[issues$Package == x, ]
        if (nrow(pkg_issues) > 0) {
            paste(pkg_issues$kind, collapse = ", ")
        } else ""
    }, character(1), USE.NAMES = FALSE)
    dplyr::mutate(tbl, "has_other_issues" = nchar(res) > 0)
}

##' @importFrom lazyeval interp
##' @importFrom stats setNames
##' @importFrom dplyr group_by_ mutate_ ungroup distinct_ select_
##' @importFrom tibble tibble
cran_details_from_crandb <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    issues <- get_cran_rds_file("issues", ...)

    dt <- dt[dt[["Package"]] == pkg, ]

    ## remove lines that don't have any issues
    dt <- dt[dt[["Check"]] != "*", ]

    dt$Status <- gsub("WARNING", "WARN", dt$Status)

    flvr <- setNames(list(lazyeval::interp(~paste(coln, collapse = ", "), coln = as.name("Flavor"))),
                     "flavors")
    res <- dt %>%
        dplyr::group_by_("Package", "Output") %>%
        dplyr::mutate_(.dots = flvr) %>%
        dplyr::ungroup() %>%
        dplyr::distinct_("Package", "Output", .keep_all = TRUE) %>%
        dplyr::select_(
            "Package",
            result = "Status",
            check = "Check",
            "flavors",
            message = "Output")

    attr(res, "other_issues") <- tibble::tibble(Package = pkg,
                                                has_other_issues = pkg %in% issues$Package)
    res
}
