##' @details Where does the data come from?
##'
##' The data comes from the CRAN servers. They generate RDS files that
##' contains information regarding the results of the checks for all
##' the packages, and all the flavors. This data is then used to
##' generate the web pages.
##'
##' \code{foghorn} provides access to either of these data sources. If
##' you choose \code{src = "website"} the data is scrapped from the
##' CRAN website. If you only need to check a few packages, this is a
##' good option. If you choose \code{src = "crandb"} the RDS files
##' (about 20Mb) are downloaded first from the CRAN servers.
##'
##' When choosing \code{src = "crandb"} you can also specify the
##' following options:
##'
##' \itemize{
##'
##'   \item {dest} { a folder where to store the RDS files (\code{tempdir()}
##'                  by default).}
##'
##'   \item {protocol} { either \code{https} (default) or \code{http}. }
##'
##'   \item {overwrite} { when \code{FALSE} (default), if the file exists in
##'                       \code{dest} then it will not be downloaded again. When
##'                       \code{TRUE} the file gets downloaded every time it's
##'                       needed.}
##' }
##'
##' @seealso Note that the \code{tools} package contains unexported
##'     functions that can be used to extract summary information from
##'     the check results. Specifically
##'     \code{tools:::sumarize_CRAN_check_status} is similar to
##'     \code{show_cran_results}.
