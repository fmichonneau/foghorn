fetch_cran_rds_file <- function(file = c("details", "results", "flavors"),
                                dest = tempdir(), protocol = c("https", "ftp"),
                                ...) {
    ## TODO -- need to check for internet connection

    file <- match.arg(file)
    protocol <- match.arg(protocol)
    is_ftp <- if (identical(protocol, "ftp")) "pub/R/" else character(0)
    file <- paste0("check_", file, ".rds")
    dest_files <- file.path(dest, file)
    cran_url <- paste0(protocol, "://cran.r-project.org/", is_ftp, "web/checks/", file)
    download.file(url = cran_url, destfile = dest_files, ...)
    invisible(dest_files)
}


read_cran_rds_file <- function(file) {
    res <- readRDS(file = file)
    class(res) <- c("cran_db", class(res))
    res
}

maintainer_pkg_info <- function(email, crandb) {
    if (!exists("Maintainer", crandb))
        stop("use `results` database")
    idx <- grepl(paste0("<", email, ">"), crandb[["Maintainer"]])
    res <- crandb[idx, ]
    class(res) <- c("crandb_email", class(res))
    res
}

table_cran_checks.crandb_email <- function(parsed, ...) {
    parsed %>%
        dplyr::count_(vars = c("Package", "Status")) %>%
        tidyr::spread_("Status", "n") %>%
        dplyr::bind_rows(default_cran_checks) %>%
        dplyr::ungroup()
}
