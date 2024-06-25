##' @details Where does the data come from?
##'
##' The data comes from the CRAN servers. They generate RDS files that
##' contains information regarding the results of the checks for all
##' the packages, and all the flavors. This data is then used to
##' generate the web pages.
##'
##' `foghorn` provides access to either of these data sources. If
##' you choose `src = "website"` the data is scrapped from the
##' CRAN website. If you only need to check a few packages, this is a
##' good option. If you choose `src = "crandb"` the RDS files
##' (about 20Mb) are downloaded first from the CRAN servers.
##'
##' The option `max_requests` can be used to limit how many pages will be
##' scrapped from the CRAN website. The default is set to 50, use `Inf` to
##' ignore this check. Consider using `src = "crandb"` if you need to get data
##' from many packages or maintainers. Note that this an approximation of the
##' number of requests that will be performed and there will be situations where
##' more requests than this limit will be performed to retrieve the results.
##'
##' The `deadline` column contains the date set by CRAN to fix issues with the
##' CRAN checks before the package gets archived. To limit the number of
##' requests to the CRAN mirror, the existence of a deadline is checked only for
##' packages where issues have been detected. If the dealine is not checked
##' (either because the argument `include_deadline` is set to `FALSE`, or
##' because there is no issues detected with the CRAN checks), the content of
##' the `deadline` column for the package will be set to `NA`. If the existence
##' of a deadline has been checked but no date has been set by the CRAN
##' Maintainers, the `deadline` column for the pacakge will be set to `""`. If
##' there is a deadline, the content will be a date stored as `character`.
##'
##' The value of the argument `include_deadline` can be set using the
##' local option `foghorn_include_deadline`.
##'
##' When choosing `src = "crandb"` you can also specify the
##' following options:
##'
##' * `dest`:  a folder where to store the RDS files (`tempdir()`
##'                  by default).
##' * `protocol`: either `https` or `http`.
##' * `overwrite`: when `FALSE` (default), if the file exists in
##'                `dest` then it will not be downloaded again. When
##'                `TRUE` the file gets downloaded every time it's
##'                needed.
##'
##' @seealso Note that the `tools` package contains unexported
##'     functions that can be used to extract summary information from
##'     the check results. Specifically
##'     `tools:::sumarize_CRAN_check_status` is similar to
##'     `show_cran_results`.
##' @md
