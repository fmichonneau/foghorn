fetch_cran_rds_file <- function(file = c("details", "results", "flavors", "memtest"),
                                dest = tempdir(), protocol = c("https", "ftp"),
                                overwrite = FALSE, ...) {
    ## TODO -- need to check for internet connection

    file <- match.arg(file)
    protocol <- match.arg(protocol)
    is_ftp <- if (identical(protocol, "ftp")) "pub/R/" else character(0)
    if (file != "memtest")
        file <- paste0("check_", file, ".rds")
    else
        file <- "memtest_notes.rds"
    dest_file <- file.path(dest, file)
    if (! file.exists(dest_file) && overwrite) {
        cran_url <- paste0(protocol, "://cran.r-project.org/", is_ftp, "web/checks/", file)
        download.file(url = cran_url, destfile = dest_file, ...)
    }
    invisible(dest_file)
}


read_cran_rds_file <- function(file) {
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
    idx <- grepl(paste0("<", email, ">"), crandb[["Maintainer"]])
    res <- crandb[idx, ]
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

details_cran_results <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    memtest <- get_cran_rds_file("memtest", ...)


    dt <- dt[dt[["Package"]] == pkg, ]

    ## remove lines that don't have any issues
    dt <- dt[dt[["Check"]] != "*", ]

    dt$Status <- gsub("WARNING", "WARN", dt$Status)

    res <- dt %>%
        group_by(Package, Output) %>%
        mutate(flavors = paste(Flavor, collapse = ", ")) %>%
        ungroup() %>%
        distinct(Package, Output, .keep_all = TRUE) %>%
        select(
            Package,
            result = Status,
            check = Check,
            flavors,
            message = Output)  %>%
        as.data.frame

    attr(res, "memtest") <- data_frame(Package = pkg,
                                        has_memtest_notes = exists(pkg, memtest))
    res
}
