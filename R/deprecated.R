
##' Deprecated functions provided for back compatibility.
##'
##' @title Deprecated functions
##' @param ... see documentation for \code{cran_results} and \code{summary_cran_details}
##' @export
##' @rdname check_cran_results
check_cran_results <- function(...) {
    .Deprecated("cran_results")
    cran_results(...)
}

##' @rdname summary_cran_results
show_cran_results <- function(...) {
    .Deprecated("summary_cran_details")
    summary_cran_details(...)
}
