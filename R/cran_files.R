fetch_cran_rds_file <- function(file = c("details", "results", "flavors"),
                                dest = tempdir(), protocol = c("https", "ftp"),
                                ...) {
    file <- match.arg(file)
    protocol <- match.arg(protocol)
    is_ftp <- if (identical(protocol, "ftp")) "pub/R/" else character(0)
    file <- paste0("check_", file, ".rds")
    cran_url <- paste0(protocol, "://cran.r-project.org/", is_ftp, "web/checks/", file)
    download.file(url = cran_url, destfile = file.path(dest, file), ...)
    invisible(file.path(dest, file))
}
