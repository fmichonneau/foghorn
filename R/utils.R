`%||%` <- function(x, y) {
    if (is.null(x)) return(y)
    x
}

##' @importFrom dplyr mutate_if if_else
convert_nas <- function(tbl) {
    dplyr::mutate_if(tbl, is.integer, function(x) (dplyr::if_else(is.na(x), 0L, x)))
}
