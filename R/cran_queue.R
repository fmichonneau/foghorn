parse_cran_incoming <- function(res) {
    if (length(res$content) == 0) return(NULL)
    folders <- read.table(text = rawToChar(res$content),
                          stringsAsFactors = FALSE)$V9
    tibble::tibble(
                cran_folder = basename(res$url),
                packages = folders
            )
}

parse_pkg <- function(pkg) {
    pkg <- gsub("\\.tar\\.gz$", "", pkg)
    pkg <- strsplit(pkg, "_")
    tibble::tibble(
        package = vapply(pkg, function(x) x[1], character(1)),
        version = vapply(pkg, function(x) x[2], character(1))
    )
}

##' Check where your package stands in the CRAN incoming queue.
##'
##' When submitting a package to CRAN, it undergoes a series of checks before it
##' is published and publicly available. `cran_incoming` allows you to check the
##' packages that are currently in the queue, and the folder where they are
##' located. This information could help you track your package submission. Only
##' the following folders are considered: `archive`, `inspect`, `noemail`,
##' `pending`, `pretest`, `publish`, `recheck`, `waiting`. The folders named
##' after the initials of the CRAN volunteers are not inspected.
##'
##' @section Disclaimer:
##' The information provided here is only to give you an indication of where
##' your package stands in the submission process. It can be useful to confirm
##' that your package has been correctly uploaded to CRAN. Please consult the
##' [CRAN Repository
##' Policy](https://cran.r-project.org/web/packages/policies.html) if you have
##' any questions.
##'
##' @title List packages in CRAN incoming queue.
##' @param pkg Optionally provide a vector of package name to limit the results
##'     to these packages.
##' @return A `tibble` with the following columns:
##'   - the name of the package: `package`
##'   - the version of the package: `version`
##'   - the name of the folder where the package was found: `cran_folder`
##' @examples
##' \dontrun{
##'   ## all the packages in the CRAN incoming queue
##'   cran_incoming()
##'   ## if the package `foo` is in the queue, it will appear below
##'   cran_incoming(pkg = "foo")
##' }
##' @importFrom utils read.table
##' @export
##' @md
cran_incoming <- function(pkg = NULL) {

    if (!is.null(pkg) &&
        (!is.character(pkg) || any(is.na(pkg)))) {
            stop(sQuote("pkg"), " must be a character vector.")
    }

    cran_incoming_url <- "ftp://cran.r-project.org/incoming/"

    sub_folders <- c("archive", "inspect", "pending", "pretest",
                     "publish", "recheck", "waiting")
    sub_folders <- paste0(sub_folders, "/")

    pool <- curl::new_pool()

    res_data <- list()

    success <- function(res) {
        res_data <<- c(res_data, list(res))
    }

    fail <- function(res) {
        warning("Server returned error: ", res, call. = FALSE)
    }

    for (sf in seq_along(sub_folders)) {
        curl::curl_fetch_multi(paste0(cran_incoming_url,"/", sub_folders[sf]),
                              pool = pool, done = success, fail = fail)
    }
    res_qry <- curl::multi_run(pool = pool)

    ## check for errors
    if (res_qry$error > 0) {
        warning("One of the folders didn't work.", call. = FALSE)
    }

    res <- lapply(res_data, function(x) parse_cran_incoming(x))
    res <- do.call("rbind", res)
    res <- cbind(res, parse_pkg(res$packages))

    res <- tibble::as_tibble(res[, c("package", "version", "cran_folder")])

    if (!is.null(pkg)) {
        res <- res[res$package %in% pkg, ]
    }
    res
}
