parse_winbuilder <- function(res) {
  if (length(res$content) == 0) {
    cnt <- list(
      package = character(0),
      version = as.package_version(character(0)),
      folder = character(0),
      time = as.POSIXct(character(0)),
      size = integer(0)
    )
    nr <- 0L
  } else {
    rr <- read.table(
      text = rawToChar(res$content),
      stringsAsFactors = FALSE
    )

    pkgs <- parse_pkg(rr$V4)

    datetime <- as.POSIXct(
      paste(rr$V1, rr$V2),
      format = "%m-%d-%y %I:%M%p",
      tz = "Europe/Berlin"
    )

    cnt <- list(
      package = pkgs$package,
      version = pkgs$version,
      folder = basename(res$url),
      time = datetime,
      size = rr$V3
    )
    nr <- nrow(rr)
  }

  tibble::new_tibble(
    cnt,
    nrow = nr,
    class = "fh_winbuild_q"
  )
}


##' Check whether your package is in the win-builder queue.
##'
##' To check whether your package has successfully been submitted to
##' win-builder, or to check whether there is unusual delay in processing
##' packages submitted to win-builder, `winbuilder_queue` allows you to inspect
##' the packages that are in the queue to be processed by the win-builder
##' service.
##'
##' @title Show the win-builder queue
##' @param pkg Optionally provide a vector of package names to limits the
##'   results to these packages.
##' @param folders Which folders of the win-builder queue do you want to
##'   inspect? Default: the 3 architectures win-builder provides.
##' @return A `tibble` with the following columns:
##' \describe{
##' \item{package}{package name}
##' \item{version}{package version}
##' \item{folder}{the folder indicating the R version that will be used to perform the checks}
##' \item{time}{the date and time at which the package tarball was uploaded on win-builder}
##' \item{size}{the size of the package tarball}
##' }
##' @references \itemize{
##'  \item Maëlle Salmon, 2020. "Everything you should know about WinBuilder" \url{https://blog.r-hub.io/2020/04/01/win-builder/}
##'  \item Uwe Ligges. Building and checking R source packages for Windows. \url{https://win-builder.r-project.org/}
##' }
##' @export
##' @examples
##' \dontrun{
##'   ## Get all the packages in the win-builder queue
##'   winbuilder_queue()
##'   ## Check if the 'dplyr' package is in the win-builder queue
##'   winbuilder_queue(pkg = "dplyr")
##'   ## Check which packages are in the R-devel queue
##'   winbuilder_queue(folders = "R-devel")
##' }
winbuilder_queue <- function(pkg = NULL,
                             folders = c("R-release", "R-devel", "R-oldrelease")) {
  folders <- match.arg(folders, several.ok = TRUE)

  res <- cran_ftp(
    pkg = pkg,
    folders = folders,
    url = "ftp://win-builder.r-project.org"
  )

  res <- lapply(res, parse_winbuilder)
  res <- do.call("rbind", res)

  if (!is.null(pkg)) {
    res <- res[res$package %in% pkg, ]
  }

  res
}
