api_base_url <- function() {
    "https://cranchecks.info"
}

api_call <- function(endpt, value, ...) {
    res <- httr::GET(api_base_url(), path = paste(endpt, value, sep = "/"), ...)
    check_api_res(res)
}

##' @importFrom purrr map_df
api_pkg_status <- function(pkg, ...) {
    purrr::map_df(pkg, function(p) {
               r <- api_call(endpt = "pkgs", p, ...)
               summary_pkg_res(r)
           })
}

##' @importFrom purrr map_df
api_maintainer <- function(email, ...) {
    purrr::map_df(email, function(e) {
               e <- convert_email_to_cran_format(email)
               r <- api_call(endpt = "maintainers", e, ...)
               summary_maintainer_res(r)
           })
}


##' @importFrom jsonlite fromJSON
##' @importFrom httr content
api_parse <- function(r) {
    if (grepl("application/json", r[["headers"]][["content-type"]])) {
        return(jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE))
    }
    res <- httr::content(r, as="text", encoding = "UTF-8")
    if(identical(res, "")){
        stop("No output to parse; check your query.", call. = FALSE)
    }
    res
}

check_api_res <- function(res) {
    if (res$status_code >= 400) {
        msg <- api_parse(res)
        stop("HTTP failure: ", res$status_code, " -- ", msg$error$message, call. = FALSE)
    }
    api_parse(res)
}
