`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}

`%~~%` <- function(x, y) {
  if (length(x) == 0L) {
    return(y)
  }
  x
}

convert_nas <- function(tbl, replace_with = 0L) {
  tbl[is.na(tbl)] <- replace_with
  tbl
}

convert_email_to_cran_format <- function(email) {
  ## check email
  lapply(email, function(x) {
    if (!grepl("\\@", x)) {
      stop("Malformed email address: ", sQuote(email), call. = FALSE)
    }
  })
  email <- gsub("\\@", "_at_", tolower(email))
  ##  "all characters different from letters, digits, hyphens,
  ##  underscores, colons, and periods replaced by underscores ..."
  email <- gsub("[^[:alnum:]_:.-]", "_", email)
  email
}

add_cols <- function(tbl) {
  names(tbl)[match("Package", names(tbl))] <- "package"
  to_add <- setdiff(names(default_cran_results), names(tbl))
  if (length(to_add) > 0) {
    col_to_add <- replicate(length(to_add), list(rep(0L, nrow(tbl))))
    names(col_to_add) <- to_add
    col_to_add <- as_tibble(col_to_add)
    res <- cbind(tbl, col_to_add)
    tbl <- tibble::as_tibble(res)
  }
  convert_nas(tbl)[, names(default_cran_results)]
}

check_n_requests <- function(..., max_requests) {
  if (is.infinite(max_requests)) {
    return(NULL)
  }
  elements <- list(...)
  if (sum(lengths(elements)) > max_requests) {
    stop(
      "This query would require more than ", max_requests,
      "web requests. Consider using ", sQuote("src = \"crandb\""),
      "before increasing the number of requests allowed.",
      call. = FALSE
    )
  }
}



### for devtools::release()
release_questions <- function() {
  c("Did you re-render the vignettes by running `make_vignettes()` (in `vignettes/`)?")
}
