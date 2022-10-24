new_cran_q <- function(package = character(0),
                       version = as.package_version(character(0)),
                       cran_folder = character(0),
                       time = as.POSIXct(character(0)),
                       size = character(0)) {
  stopifnot(is.character(package))
  stopifnot(is.package_version(version))
  stopifnot(is.character(cran_folder))
  stopifnot(inherits(time, "POSIXct"))
  stopifnot(is.character(size))

  tibble::tibble(
    package,
    version,
    cran_folder,
    time,
    size
  )
}

parse_http_cran_incoming <- function(res_raw) {
  if (identical(length(res_raw$content), 0L)) {
    return(new_cran_q())
  }

  res <- rawToChar(res_raw$content)
  res <- xml2::read_html(res)
  res <- rvest::html_table(res)

  if (!identical(length(res), 1L)) {
    stop(
      "Issue with formatting of table. Please report the bug.",
      call. = FALSE
    )
  }

  res <- res[[1]]

  res <- res[nzchar(res$Name) & res$Name != "Parent Directory", , drop = FALSE]

  if (identical(nrow(res), 0L)) {
    return(new_cran_q())
  }

  pkgs <- parse_pkg(res$Name)
  time <- as.POSIXct(res[["Last modified"]], tz = "Europe/Vienna")
  size <- res$Size

  new_cran_q(
    package = pkgs$package,
    version = pkgs$version,
    cran_folder = basename(res_raw$url),
    time = time,
    size = size
  )
}

parse_pkg_version <- function(pkg) {
  vapply(
    pkg, function(x) {
      x[2]
    },
    character(1)
  )
}

parse_pkg <- function(pkg) {
  pkg <- gsub("\\.tar\\.gz$", "", pkg)
  pkg <- strsplit(pkg, "_")
  tibble::tibble(
    package = vapply(pkg, function(x) x[1], character(1)),
    version = package_version(parse_pkg_version(pkg), strict = FALSE)
  )
}

## internal function to scrape content of CRAN queue servers, used by
## `cran_incoming` and `winbuilder_queue`.
cran_queue <- function(pkg, folders, url) {
  if (!is.null(pkg) &&
    (!is.character(pkg) || any(is.na(pkg)))) {
    stop(sQuote("pkg"), " must be a character vector.", call. = FALSE)
  }

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
    curl::curl_fetch_multi(paste0(
      url, "/", sub_folders[sf]
    ),
    pool = pool, done = success, fail = fail
    )
  }
  res_qry <- curl::multi_run(pool = pool)

  ## check for errors
  if (res_qry$error > 0) {
    warning("One of the folders didn't work.", call. = FALSE)
  }

  res_data
}


##' Check where your package stands in the CRAN incoming queue.
##'
##' When submitting a package to CRAN, it undergoes a series of checks
##' before it is published and publicly available. `cran_incoming()`
##' allows you to check the packages that are currently in the queue,
##' and the folder where they are located. This information could help
##' you track your package submission. Only the following folders are
##' considered (approximately in order of the CRAN queue sequence):
##' `newbies`, `inspect`, `pretest`, `recheck`, `pending`, `waiting`,
##' `publish`. The folder `archive` is not inspected by default. The
##' folders named after the initials of the CRAN volunteers are not
##' inspected.
##'
##' @note
##' The meaning of the package folders is as follows
##'  (see Hornik, Ligges and Zeileis \url{https://journal.r-project.org/archive/2018-1/cran.pdf} and Uwe Ligges mailing list comment \url{https://stat.ethz.ch/pipermail/r-package-devel/2019q1/003631.html}):
##' \describe{
##' \item{newbies}{for first time submission; package will be manually inspected.}
##' \item{inspect}{package is awaiting manual inspection; always happens for first time submissions and for packages with problems that are likely to be false positives}
##' \item{pretest}{a human has triggered a new auto-check of the package}
##' \item{recheck}{package has passed checks and is waiting for reverse dependency checking}
##' \item{pending}{a CRAN team member has to do a closer inspection and needs more time}
##' \item{waiting}{CRAN's decision is waiting for a response from the package maintainer, e.g. when issues are present that CRAN cannot check for in the incoming checks}
##' \item{publish}{package is awaiting publication}
##' \item{archive}{package rejected: it does not pass the checks cleanly and the problems are unlikely to be false positives}
##' }
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
##' @param pkg Optionally provide a vector of package names to limit
##'   the results to these packages.
##' @param folders Which folders of the CRAN queue do you want to
##'   inspect? Default: all the non-human folders.
##' @param include_archive when `TRUE`, the function
##'   `cran_incoming_folders()` also returns the `archive` folder.
##' @param sort_by_date when `TRUE` (default), the output is sorted in
##'   decreasing order according to the submission time.
##' @return `cran_incoming()` returns `tibble` with the following
##'   columns: \describe{ \item{package}{package name}
##'   \item{version}{package version} \item{cran_folder}{folder where
##'   the package was found} \item{time}{date/time package was entered
##'   in the folder} \item{size}{the size of the package tarball} }
##'
##'   `cran_incoming_folders()` returns a character vector of the
##'   names of the folders used as part of the CRAN submission
##'   process, `archive` being included optionally.
##'   
##'
##' Note that if the package version is not provided, it will appear as `NA`
##' in the `tibble`.
##'
##' @examples
##' \dontrun{
##'   ## all the packages in the CRAN incoming queue
##'   cran_incoming()
##'   ## to include all the folders including `archive`
##'   cran_incoming(folders = cran_incoming_folders(include_archive = TRUE)
##'   ## to only include a folder, e.g., `inspect`
##'   cran_incoming(folders = "inspect")
##'   ## if the package `foo` is in the queue, it will appear below
##'   cran_incoming(pkg = "foo")
##' }
##' @references
##' \itemize{
##' \item Hornik, Ligges and Zeileis. "Changes on CRAN: 2017-12-01 to 2018-06-30", R Journal 10(1), July 2018. \url{https://journal.r-project.org/archive/2018-1/cran.pdf}
##' \item  Maëlle Salmon, Locke Data, Stephanie Locke, Mitchell O'Hara-Wild, Hugo Gruson. "CRAN incoming dashboard", \url{https://lockedata.github.io/cransays/articles/dashboard.html}
##' }
##' @seealso cran_winbuilder
##' @importFrom utils read.table
##' @export
##' @md
cran_incoming <- function(pkg = NULL,
                          folders = cran_incoming_folders(),
                          sort_by_date = TRUE
                          ) {
  
  folders <- match.arg(
    folders,
    cran_incoming_folders(include_archive = TRUE),
    several.ok = TRUE
  )

  res_data <- cran_queue(
    pkg = pkg,
    folders = folders,
    url = "https://cran.r-project.org/incoming/"
  )

  res <- lapply(res_data, parse_http_cran_incoming)
  res <- do.call("rbind", res)
  
  if (!is.null(pkg)) {
    res <- res[res$package %in% pkg, ]
  }

  if (sort_by_date) {
    res <- res[order(res$time, decreasing = TRUE), ]
  }
  
  res
}

##' @export
##' @rdname cran_incoming
cran_incoming_folders <- function(include_archive = FALSE) {
  folders <- c(
    "newbies",
    "inspect",
    "pretest",
    "recheck",
    "pending",
    "publish",
    "waiting"
  )

  if (include_archive) {
    folders <- c(folders, "archive")
  }
  
  folders
  
}
