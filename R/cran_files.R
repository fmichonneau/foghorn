##' @importFrom httr2 request req_options req_perform_parallel
fetch_cran_rds_file <- function(
  file = c("details", "results", "flavors", "issues", "packages"),
  dest = tempdir(),
  protocol = c("https", "http"),
  overwrite = FALSE,
  file_prefix = NULL,
  ...
) {
  file <- match.arg(file)
  protocol <- match.arg(protocol)
  if (file == "packages") {
    file <- paste0(file, ".rds")
  } else {
    file <- paste0("check_", file, ".rds")
  }
  dest_file <- file.path(dest, paste0(file_prefix, file))

  if (
    !(file.exists(dest_file) &&
      file.info(dest_file, extra_cols = FALSE)$size > 0) ||
      overwrite
  ) {
    if (file == "packages.rds") {
      file_cran_url <- paste0(cran_url(protocol), "/web/packages/", file)
    } else {
      file_cran_url <- paste0(cran_url(protocol), "/web/checks/", file)
    }

    req <- httr2::request(file_cran_url)
    req <- httr2::req_options(req)

    ## we use req_perform_parallel even though we do a single request
    ## because it always succeed.
    resp <- httr2::req_perform_parallel(
      list(req),
      paths = dest_file,
      on_error = "return",
      progress = FALSE
    )[[1]]

    if (inherits(resp, "error")) {
      unlink(dest_file)
      stop("Cannot access ", file_cran_url, "\nMessage: ", dQuote(resp$message))
    }
  }

  invisible(dest_file)
}

check_cran_rds_file <- function(file, return_logical = FALSE) {
  if (!file.exists(file)) {
    stop(file, " can't be found...", call. = FALSE)
  }
  if (file.info(file, extra_cols = FALSE)$size < 100) {
    stop(file, " is corrupted. Delete it and retry.")
  }
  res <- try(readRDS(file = file), silent = TRUE)
  if (inherits(res, "try-error")) {
    stop(file, " can't be read. Delete it and retry.")
  } else if (return_logical) {
    return(TRUE)
  } else {
    res
  }
}

read_cran_rds_file <- function(file) {
  res <- check_cran_rds_file(file)
  names(res) <- tolower(names(res))
  class(res) <- c("cran_db", class(res))
  res
}

## file can be a valid RDS file downloaded manually (or using a custom
## fetch_cran_rds_file() call), or the type of file that one wants to
## download from CRAN servers (e.g., "results", "details", "issues", ...)
get_cran_rds_file <- function(file, ...) {
  f <- fetch_cran_rds_file(file, ...)
  read_cran_rds_file(f)
}

read_crandb_from_email <- function(email, file = "results", ...) {
  crandb <- get_cran_rds_file(file = file, ...)
  maintainer <- tolower(crandb$maintainer)
  idx <- lapply(tolower(email), function(x) {
    grepl(paste0("<", x, ">"), maintainer, fixed = TRUE)
  })

  check_no_email_match(idx, email)
  idx <- Reduce("+", idx)

  res <- crandb[as.logical(idx), ]
  class(res) <- c("crandb", class(res))
  res
}

read_crandb_from_pkg <- function(pkg, file = "results", ...) {
  crandb <- get_cran_rds_file(file = file, ...)
  res <- crandb[crandb[["package"]] %in% pkg, ]
  class(res) <- c("crandb", class(res))
  res
}

add_other_issues_crandb <- function(tbl, ...) {
  issues <- get_cran_rds_file("issues", ...)
  res <- vapply(
    tbl[["package"]],
    function(x) {
      pkg_issues <- issues[issues$package == x, ]
      if (nrow(pkg_issues) > 0) {
        paste(pkg_issues$kind, collapse = ", ")
      } else {
        ""
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
  tbl$has_other_issues <- nchar(res) > 0
  tbl
}
