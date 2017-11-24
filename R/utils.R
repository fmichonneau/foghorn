`%||%` <- function(x, y) {
    if (is.null(x)) return(y)
    x
}

`%~~%` <- function(x, y) {
    if (length(x) == 0L) return(y)
    x
}

convert_nas <- function(tbl, replace_with = 0L) {
    tbl[is.na(tbl)] <- replace_with
    tbl
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

add_cols <- function(tbl) {
    to_add <- setdiff(names(default_cran_results), names(tbl))
    if (length(to_add) > 0) {
        col_to_add <- replicate(length(to_add), list(rep(0L, nrow(tbl))))
        names(col_to_add) <- to_add
        col_to_add <- as.tibble(col_to_add)
        res <- cbind(tbl, col_to_add)
        tbl <- tibble::as.tibble(res)
    }
    convert_nas(tbl)[, c("Package", names(default_cran_results))]
}
