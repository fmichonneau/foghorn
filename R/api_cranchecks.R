api_base_url <- function() {
    "https://cranchecks.info"
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
