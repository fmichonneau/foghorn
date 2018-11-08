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
##' is published and publicly available. `cran_incoming()` allows you to check
##' the packages that are currently in the queue, and the folder where they are
##' located. This information could help you track your package submission. Only
##' the following folders are considered (approximately in order of the CRAN
##' queue sequence): `inspect`, `pretest`, `recheck`, `pending`, `publish`, `archive`.
##' The folders named after the initials of the CRAN volunteers are not inspected.
##'
##' @note
##' The meaning of the package folders is as follows (see
##' Hornik, Ligges and Zeileis \url{https://journal.r-project.org/archive/2018-1/cran.pdf}):
##' \describe{
##' \item{inspect}{package is awaiting manual inspection; always happens for first time submissions and for packages with problems that are likely to be false positives}
##' \item{pretest}{a human has triggered a new auto-check of the package}
##' \item{recheck}{package has passed checks and is waiting for reverse dependency checking}
##' \item{pending}{CRAN's decision is waiting for a response from the package maintainer, e.g. when issues are present that CRAN cannot check for in the incoming checks}
##' \item{publish}{package is awaiting publication}
##' \item{archive}{package rejected: it does not pass the checks cleanly and the problems are unlikely to be false positives}
##' }
##' https://cransays.itsalocke.com/articles/dashboard.html
##' @section Disclaimer:
##' The information provided here is only to give you an indication of where
##' your package stands in the submission process. It can be useful to confirm
##' that your package has been correctly uploaded to CRAN. Please consult the
##' [CRAN Repository
##' Policy](https://cran.r-project.org/web/packages/policies.html) if you have
##' any questions.
##'
##' @title List packages in CRAN incoming queue.
##' @param pkg Optionally provide a vector of package names to limit the results
##'     to these packages.
##' @param folders Which folders of the CRAN FTP site do you want to inspect? Default: 
##'     all the non-human folders.
##' @return A `tibble` with the following columns:
##' - the name of the package: `package`
##' - the version of the package: `version`
##' - the name of the folder where the package was found: `cran_folder`
##' @examples
##' \dontrun{
##'   ## all the packages in the CRAN incoming queue
##'   cran_incoming()
##'   ## if the package `foo` is in the queue, it will appear below
##'   cran_incoming(pkg = "foo")
##' }
##' @references
##' \itemize{
##' \item Hornik, Ligges and Zeileis. "Changes on CRAN: 2017-12-01 to 2018-06-30", R Journal 10(1), July 2018. \url{https://journal.r-project.org/archive/2018-1/cran.pdf}
##' \item  Maëlle Salmon, Locke Data, Stephanie Locke, Mitchell O'Hara-Wild, Hugo Gruson. "CRAN incoming dashboard", \url{https://cransays.itsalocke.com/articles/dashboard.html}
##' }
##' @importFrom utils read.table
##' @export
##' @md
cran_incoming <- function(pkg = NULL,
                          folders =  c("inspect", "pretest", "recheck", "pending", "publish", "archive")) {

    if (!is.null(pkg) &&
        (!is.character(pkg) || any(is.na(pkg)))) {
            stop(sQuote("pkg"), " must be a character vector.")
    }

    folders <- match.arg(folders, several.ok = TRUE)

    cran_incoming_url <- "ftp://cran.r-project.org/incoming/"

    sub_folders <- paste0(folders, "/")

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
    res$cran_folder <- factor(res$cran_folder, levels=folders)

    if (!is.null(pkg)) {
        res <- res[ res$package %in% pkg, ]
    }
    res
}
