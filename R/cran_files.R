## progress_multi taken from `provisionr` written by Rich FitzJohn:
##   https://github.com/mrc-ide/provisionr/blob/master/R/download.R#L124-L179
##
progress_multi <- function(i, labels, count, progress) {
  label <- format(labels[[i]], width = max(nchar(labels)), justify = "right")
  if (count) {
    is <- format(i, width = nchar(length(labels)))
    prefix <- sprintf("[%s/%s] %s", is, length(labels), label)
  } else {
    prefix <- label
  }
  bar <- NULL
  type <- "down"
  seen <- 0

  if (progress) {
    callback <- function(down, up) {
      if (type == "down") {
        total <- down[[1L]]
        now <- down[[2L]]
      } else {
        total <- up[[1L]]
        now <- up[[2L]]
      }

      if (total == 0 && now == 0) {
        bar <<- NULL
        seen <<- 0
        return(TRUE)
      }

      if (is.null(bar)) {
        if (total == 0) {
          fmt <- paste0(prefix, " [ :bytes in :elapsed ]")
          total <- 1e8 # arbitrarily big
        } else {
          fmt <- paste0(prefix, " [:percent :bar]")
        }
        bar <<- progress::progress_bar$new(fmt, total,
          clear = TRUE,
          show_after = 0
        )
      }
      if (total == 0) {
        bar$tick(now)
      } else {
        bar$tick(now - seen)
        seen <<- now
      }

      TRUE
    }
  } else {
    callback <- function(down, up) {
      TRUE
    }
  }

  list(
    callback = callback,
    prefix = prefix
  )
}


##' @importFrom httr GET write_disk status_code config
fetch_cran_rds_file <- function(file = c("details", "results", "flavors", "issues"),
                                dest = tempdir(), protocol = c("https", "http"),
                                overwrite = FALSE, file_prefix = NULL,
                                progress = TRUE, ...) {
  file <- match.arg(file)
  protocol <- match.arg(protocol)
  file <- paste0("check_", file, ".rds")
  dest_file <- file.path(dest, paste0(file_prefix, file))
  if (!(file.exists(dest_file) &&
    file.info(dest_file, extra_cols = FALSE)$size > 0) ||
    overwrite) {
    file_cran_url <- paste0(cran_url(protocol), "/web/checks/", file)
    if (interactive()) {
      pb <- progress_multi(
        i = 1, labels = list(paste("Downloading", file)), count = FALSE,
        progress = requireNamespace("progress", quietly = TRUE) && progress
      )
    } else {
      pb <- NULL
    }
    d_status <- httr::GET(
      url = file_cran_url,
      httr::write_disk(dest_file, overwrite = overwrite),
      httr::config(progressfunction = pb$callback), ...
    )

    if (!identical(httr::status_code(d_status), 200L)) {
      unlink(dest_file)
      stop("Can't get ", file_cran_url, " (status code: ", httr::status_code(d_status), ")", call. = FALSE)
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
  if (grepl("\\.rds$", file, ignore.case = TRUE) &&
    file.exists(file)) {
    f <- file
  } else {
    f <- fetch_cran_rds_file(file, ...)
  }
  read_cran_rds_file(f)
}

read_crandb_from_email <- function(email, file = "results", ...) {
  crandb <- get_cran_rds_file(file = file, ...)
  maintainer <- tolower(crandb$maintainer)
  idx <- lapply(tolower(email), function(x) {
    grepl(paste0("<", x, ">"), maintainer, fixed = TRUE)
  })
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
  res <- vapply(tbl[["package"]], function(x) {
    pkg_issues <- issues[issues$package == x, ]
    if (nrow(pkg_issues) > 0) {
      paste(pkg_issues$kind, collapse = ", ")
    } else {
      ""
    }
  }, character(1), USE.NAMES = FALSE)
  tbl$has_other_issues <- nchar(res) > 0
  tbl
}
