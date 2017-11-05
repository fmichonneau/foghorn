`%||%` <- function(x, y) {
    if (is.null(x)) return(y)
    x
}

##' @importFrom dplyr mutate_if if_else
convert_nas <- function(tbl) {
    dplyr::mutate_if(tbl, is.integer, function(x) (dplyr::if_else(is.na(x), 0L, x)))
}

convert_email_to_cran_format <- function(email) {
    ## check email
    lapply(email, function(x)
        if (!grepl("\\@", x)) {
            stop("Malformed email address: ", sQuote(email), call. = FALSE)
        })
    email <- gsub("\\@", "_at_", tolower(email))
    ##  "all characters different from letters, digits, hyphens,
    ##  underscores, colons, and periods replaced by underscores ..."
    email <- gsub("[^[:alnum:]_:.-]", "_", email)
    email
}
