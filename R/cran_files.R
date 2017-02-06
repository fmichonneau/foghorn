##' @importFrom httr GET write_disk status_code
fetch_cran_rds_file <- function(file = c("details", "results", "flavors", "memtest"),
                                dest = tempdir(), protocol = c("http", "https"),
                                overwrite = FALSE, ...) {
    file <- match.arg(file)
    protocol <- match.arg(protocol)
    is_ftp <- if (identical(protocol, "ftp")) "pub/R/" else character(0)
    if (file != "memtest")
        file <- paste0("check_", file, ".rds")
    else
        file <- "memtest_notes.rds"
    dest_file <- file.path(dest, file)
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


read_cran_rds_file <- function(file) {
    if (!file.exists(file))
        stop(file, " can't be found...", call. = FALSE)
    if (file.size(file) < 100)
        stop(file, " is corrupted. Delete it and retry")
    res <- readRDS(file = file)
    class(res) <- c("cran_db", class(res))
    res
}

get_cran_rds_file <- function(file, ...) {
    f <- fetch_cran_rds_file(file, ...)
    read_cran_rds_file(f)
}

crandb_pkg_info_email <- function(email, ...) {
    crandb <- get_cran_rds_file(file = "results", ...)
    maintainer <- tolower(crandb$Maintainer)
    idx <- lapply(tolower(email), function(x)
        grepl(paste0("<", x, ">"), maintainer, fixed = TRUE))
    idx <- Reduce("+", idx)

    res <- crandb[as.logical(idx), ]
    class(res) <- c("crandb", class(res))
    res
}

crandb_pkg_info_pkg <- function(pkg, ...) {
    crandb <- get_cran_rds_file(file = "results", ...)
    res <- crandb[crandb[["Package"]] %in% pkg, ]
    class(res) <- c("crandb", class(res))
    res
}

table_cran_checks.crandb <- function(parsed, ...) {
    parsed$Status <- gsub("WARNING", "WARN", parsed$Status)
    parsed %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup()
}


add_memtest_crandb <- function(tbl, ...) {
    memtest <- get_cran_rds_file("memtest", ...)
    res <- vapply(tbl[["Package"]], function(x) exists(x, memtest), logical(1),
                  USE.NAMES = FALSE)
    dplyr::mutate(tbl, "has_memtest_notes" = res)
}

##' @importFrom lazyeval interp
##' @importFrom stats setNames
##' @importFrom dplyr group_by_ mutate_ ungroup distinct_ select_
details_cran_results <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    memtest <- get_cran_rds_file("memtest", ...)


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

    attr(res, "memtest") <- tibble::data_frame(Package = pkg,
                                               has_memtest_notes = exists(pkg, memtest))
    res
}
