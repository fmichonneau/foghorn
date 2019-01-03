##' Visit the page in your web browser for a given package or a
##' maintainer's email address
##'
##' @title Visit the CRAN check results page
##' @param pkg name of the package to check the results for
##' @param email email address of the package maintainer
##' @return The URL from the CRAN check results page invisibly
##' @export
##' @importFrom utils browseURL
visit_cran_check <- function(pkg = NULL, email = NULL) {
  if (is.null(pkg) && is.null(email)) {
    stop("A package name or an email address needs to be specified",
      call. = FALSE
    )
  }
  if (!is.null(pkg) && !is.null(email)) {
    stop("Specify only one package or one email address", call. = FALSE)
  }
  if (!is.null(pkg)) {
    if (is.character(pkg) && length(pkg) == 1) {
      url <- url_pkg_res(pkg)
    } else {
      stop(sQuote("pkg"), " must be a string")
    }
  } else if (!is.null(email)) {
    if (is.character(email) && length(email) == 1) {
      url <- url_email_res(email)
    } else {
      stop(sQuote("email"), " must be a string")
    }
  }
  if (interactive()) {
    utils::browseURL(url)
  } else {
    warning("This function is only available in interactive mode.")
  }
  invisible(url)
}
