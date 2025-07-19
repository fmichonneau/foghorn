url_pkg <- function(pkg) {
  paste0(cran_url(), "/web/packages/", pkg, "/index.html")
}

.internal_read_pkg_page <- function(url) {
  stopifnot(identical(length(url), 1L))
  on.exit(clean_connection(url), add = TRUE)
  req <- httr2::request(url)
  httr2::req_perform_parallel(
    list(req),
    on_error = "continue",
    progress = FALSE
  )[[1]]
}

read_pkg_page <- function(url) {
  if (!curl::has_internet()) {
    stop("No internet connection detected", call. = FALSE)
  }
  retry_connect(.internal_read_pkg_page(url))
}

read_pkg_pages <- function(pkg) {
  url <- url_pkg(pkg)
  res <- lapply(url, read_pkg_page)
  handle_cran_web_issues(
    pkg,
    res,
    "Invalid package name(s): ",
    "Something went wrong with getting data for package name(s): "
  )
  res <- lapply(res, function(x) {
    xml2::read_html(httr2::resp_body_raw(x))
  })
  res
}

extract_deadline <- function(parsed, ...) {
  vapply(
    parsed,
    function(x) {
      needs_fix <- xml2::xml_find_all(x, ".//tr//td//span[@style]")
      needs_fix <- xml2::xml_text(needs_fix)
      if (identical(length(needs_fix), 0L)) {
        return(NA_character_)
      }
      if (!grepl("issues need fixing before", needs_fix)) {
        warning("Unrecognized value: ", needs_fix, call. = FALSE)
        return(NA_character_)
      }
      date_match <- regexpr("\\d{4}-\\d{2}-\\d{2}", needs_fix)
      regmatches(needs_fix, date_match)
    },
    character(1)
  )
}

deadline_pkg_web <- function(pkg, include_deadline, max_requests) {
  if (!include_deadline) {
    return(
      tibble(
        package = pkg,
        deadline = rep(NA_character_, length(pkg))
      )
    )
  }
  check_n_requests(pkg, max_requests = max_requests)
  res <- read_pkg_pages(pkg)
  res <- extract_deadline(res)
  res <- .mapply(
    function(.pkg, .res) {
      tibble::tibble(
        package = .pkg,
        deadline = .res
      )
    },
    list(pkg, res),
    NULL
  )
  do.call("rbind", res)
}

##' @importFrom tibble tibble
deadline_pkg_crandb <- function(pkg, ...) {
  pkgs <- as.data.frame(
    get_cran_rds_file("packages", ...),
    stringsAsFactors = FALSE
  )
  res <- pkgs[pkgs$Package %in% pkg, c("Package", "Deadline")]

  tibble::tibble(
    package = res$Package,
    deadline = res$Deadline
  )
}

check_no_email_match <- function(idx, email) {
  check_no_match <- vapply(idx, sum, integer(1))
  if (any(check_no_match < 1)) {
    stop(
      "No package found for: ",
      paste(email[check_no_match < 1], collapse = ", "),
      call. = FALSE
    )
  }
}

##' @importFrom tibble tibble
deadline_email_crandb <- function(email, ...) {
  pkgs <- as.data.frame(
    get_cran_rds_file("packages", ...),
    stringsAsFactors = FALSE
  )
  maintainer <- tolower(pkgs$Maintainer)
  idx <- lapply(tolower(email), function(x) {
    grepl(paste0("<", x, ">"), maintainer, fixed = TRUE)
  })

  check_no_email_match(idx, email)
  idx <- Reduce("+", idx)

  res <- pkgs[as.logical(idx), c("Package", "Deadline")]
  tibble::tibble(
    package = res$Package,
    deadline = res$Deadline
  )
}

add_deadline <- function(res, fix) {
  tibble::as_tibble(merge(res, fix, by = "package", all = TRUE))
}
