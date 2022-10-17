cran_url <- function(protocol = "https") {
  protocol <- match.arg(protocol, c("https", "http"))

  mirror <- getOption("repos")[["CRAN"]][1]

  if (is.na(mirror) || identical(mirror, "@CRAN@")) {
    mirror <- "://cloud.r-project.org"
  } else {
    mirror <- paste0("://", xml2::url_parse(mirror)[["server"]])
  }

  paste0(protocol, mirror)
}

url_pkg_res <- function(pkg) {
  paste0(cran_url(), "/web/checks/check_results_", pkg, ".html")
}

url_email_res <- function(email) {
  email <- convert_email_to_cran_format(email)
  paste0(
    cran_url(), "/web/checks/check_results_",
    email, ".html"
  )
}

clean_connection <- function(x) {
  ss <- showConnections(all = TRUE)
  cc <- as.numeric(rownames(ss)[ss[, 1] == x])
  if (length(cc) > 0) on.exit(close(getConnection(cc)))
}

.internal_read_cran_web <- function(x) {
  on.exit(clean_connection(x))
  xml2::read_html(x)
}

retry_connect <- function(f, n_attempts = 3) {
  res <- try(f, silent = TRUE)
  attempts <- 0
  pred <- inherits(res, "try-error")

  if (pred && grepl("HTTP error 404", res)) {
    return(res)
  }

  while (pred && attempts < n_attempts) {
    Sys.sleep(exp(stats::runif(1) * attempts))
    res <- try(f, silent = TRUE)
    pred <- inherits(res, "try-error")
    attempts <- attempts + 1
  }

  res
}


##' @importFrom xml2 read_html
##' @importFrom curl has_internet
read_cran_web <- function(x) {
  if (!curl::has_internet()) {
    stop("No internet connection detected", call. = FALSE)
  }
  retry_connect(.internal_read_cran_web(x))
}

handle_cran_web_issues <- function(input, res, msg_404, msg_other) {
  if (length(bad <- grep("HTTP error 404", res)) > 0) {
    stop(msg_404,
      paste(sQuote(input[bad]), collapse = ", "),
      ".\n", "Original error: ", sQuote(res[bad]),
      call. = FALSE
    )
  }
  if (length(bad <- which(vapply(res, inherits, logical(1), "try-error")))) {
    stop(msg_other,
      paste(sQuote(input[bad]), collapse = ", "), "\n",
      "  ", res[[bad]],
      call. = FALSE
    )
  }
}

read_cran_web_from_email <- function(email) {
  url <- url_email_res(email)
  res <- lapply(url, read_cran_web)
  handle_cran_web_issues(
    email, res,
    "Invalid email address(es): ",
    "Something went wrong with getting data with email address(es): "
  )
  class(res) <- c("cran_checks_email", class(res))
  res
}

read_cran_web_from_pkg <- function(pkg) {
  url <- url_pkg_res(pkg)
  res <- lapply(url, read_cran_web)
  handle_cran_web_issues(
    pkg, res,
    "Invalid package name(s): ",
    "Something went wrong with getting data for package name(s): "
  )
  names(res) <- pkg
  class(res) <- c("cran_checks_pkg", class(res))
  res
}

##' @importFrom tibble as_tibble
get_cran_table <- function(parsed, ...) {
  res <- lapply(parsed, function(x) {
    tbl <- rvest::html_table(x)[[1]]
    names(tbl) <- tolower(names(tbl))
    tbl$version <- as.character(tbl$version)
    tbl
  })
  names(res) <- names(parsed)
  pkg_col <- rep(names(res), vapply(res, nrow, integer(1)))
  res <- do.call("rbind", res)
  res <- cbind(package = pkg_col, res, stringsAsFactors = FALSE)
  tibble::as_tibble(res)
}


all_packages <- function(parsed, ...) UseMethod("all_packages")

all_packages_by_email <- function(x) {
  xml2::xml_text(xml2::xml_find_all(x, ".//h3/@id"))
}

all_packages.cran_checks_email <- function(parsed, ...) {
  lapply(parsed, all_packages_by_email)
}

all_packages.cran_checks_pkg <- function(parsed, ...) {
  lapply(parsed, function(x) {
    res <- xml2::xml_find_all(x, ".//h2/a/text()")
    gsub("\\s", "", xml2::xml_text(res))
  })
}

##' @importFrom xml2 xml_find_all xml_text
##' @importFrom tibble tibble
has_other_issues <- function(parsed, ...) {
  pkg <- all_packages(parsed)

  res <- lapply(pkg, function(x) {
    tibble::tibble(
      `package` = x,
      `has_other_issues` = rep(FALSE, length(x))
    )
  })

  res <- do.call("rbind", res)
  pkg_with_issue <- lapply(parsed, function(x) {
    all_urls <- xml2::xml_find_all(x, ".//h3//child::a[@href]//@href")
    all_urls <- xml2::xml_text(all_urls)
    with_issue <- grep("check_issue_kinds", all_urls, value = TRUE)
    pkg_with_issue <- unique(basename(with_issue))
    if (length(pkg_with_issue) == 0) {
      return(NULL)
    }
    TRUE
  })
  pkg_with_issue <- unlist(pkg_with_issue)
  res[["has_other_issues"]][match(names(pkg_with_issue), res$package)] <- TRUE
  res
}

##' @importFrom tibble tibble
add_other_issues <- function(tbl, parsed, ...) {
  other_issues <- has_other_issues(parsed)
  tibble::as_tibble(merge(tbl, other_issues, by = "package"))
}

print_all_clear <- function(pkgs) {
  message(crayon::green(paste0(
    clisymbols::symbol$tick, " All clear for ",
    paste0(pkgs, collapse = ", "), "!"
  )))
}

get_pkg_with_results <- function(tbl_pkg,
                                 what,
                                 compact = FALSE,
                                 print_ok, ...) {
  what <- match.arg(what, names(tbl_pkg)[-1])

  if (identical(what, "ok")) {
    pkg_all_clear <- tbl_pkg[["package"]][tbl_pkg[["ok"]] == n_cran_flavors()]
    if (length(pkg_all_clear) && print_ok) {
      print_all_clear(pkg_all_clear)
    }
    return(NULL)
  }

  if (what %in% c("has_other_issues")) {
    show_n <- FALSE
  } else {
    show_n <- TRUE
  }
  if (sum(tbl_pkg[[what]], na.rm = TRUE) > 0) {
    n <- tbl_pkg[[what]][tbl_pkg[[what]] > 0]
    if (show_n) {
      n <- paste0(" (", n, ")")
    } else {
      n <- character(0)
    }
    if (compact) {
      sptr <- c("", ", ")
    } else {
      sptr <- c("  - ", "\n")
    }
    res <- paste0(sptr[1], tbl_pkg$package[!is.na(tbl_pkg[[what]]) &
      tbl_pkg[[what]] > 0],
    n,
    collapse = sptr[2]
    )
  } else {
    res <- NULL
  }
  print_summary_cran(what, res, compact)
}

print_summary_cran <- function(type = c(
                                 "ok", "error", "fail", "warn",
                                 "note", "has_other_issues"
                               ),
                               pkgs, compact) {
  if (is.null(pkgs)) {
    return(NULL)
  }

  type <- match.arg(type)
  if (compact) {
    nl <- character(0)
  } else {
    nl <- "\n"
  }

  if (grepl(",|\\n", pkgs)) {
    pkg_string <- "Packages"
  } else {
    pkg_string <- "Package"
  }

  msg <- paste(
    " ", pkg_string, "with", foghorn_components[[type]]$word,
    "on CRAN: "
  )
  message(foghorn_components[[type]]$color(
    paste0(
      foghorn_components[[type]]$symbol,
      msg, nl,
      crayon::bold(pkgs)
    )
  ))
}
