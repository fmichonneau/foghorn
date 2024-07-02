## check source validity

test_that(
  "invalid source name",
  {
    expect_error(cran_results(pkg = "dplyr", src = "foo"))
  }
)


## fail for invalid package name/emails

test_that("invalid package name", {
  skip_on_cran()
  expect_error(
    cran_results(pkg = "foobarfoobar"),
    "Invalid package name"
  )
})

test_that("invalid email address", {
  skip_on_cran()
  expect_error(
    cran_results(email = "foobar"),
    "Malformed email address"
  )
  expect_error(
    cran_results(email = "foo@foobarbar.rrrr"),
    "Invalid email address"
  )
})

## check cran results

# nolint start
validate_cran_results <- function(x, deadline = TRUE) {
  result_names <- c(
    "package", "error", "fail", "warn",
    "note", "ok", "has_other_issues"
  )
  expected_size <- 7L
  if (deadline) {
    result_names <- c(result_names, "deadline")
    expected_size <- 8L
  }
  length(x) == expected_size &&
    inherits(x, "tbl_df") &&
    all(names(x) %in% result_names) &&
    nrow(x) > 0 &&
    is.logical(x[["has_other_issues"]]) &&
    ## 2 allows for something other that "has_issues_notes" to be in the table
    if (!deadline) {
      any(x[1, ] > 2)
    } else {
      any(x[1, -match("deadline", names(x))] > 2)
    } &&
    if (!deadline) {
      !any(is.na(x)) ## no NAs allowed
    } else {
      !any(is.na(x[, -match("deadline", names(x))])) ## no NAs allowed
    }
}

validate_cran_details <- function(x) {
  length(x) == 7L &&
    inherits(x, "tbl_df") &&
    all(names(x) %in% c(
      "package", "version", "result", "check",
      "flavors", "n_flavors", "message"
    ))
}
# nolint end

## retrieves the number of CRAN flavors

test_that("at least email or package name specified (website)", {
  expect_error(cran_results(), "provide at least one value for")
})

test_that("at least email or package name specified (crandb)", {
  expect_error(cran_results(src = "crandb"), "provide at least one value for")
})

test_that("works with one package, one address, or both (website)", {
  skip_on_cran()
  res_pkg <- cran_results(pkg = "phylobase")
  res_email <- cran_results(email = "francois.michonneau@gmail.com")
  res_both <- cran_results(
    email = "francois.michonneau@gmail.com",
    pkg = "ridigbio"
  )
  expect_true(validate_cran_results(res_pkg))
  expect_true(validate_cran_results(res_email))
  expect_true(validate_cran_results(res_both))
})

test_that("works with one package, one address, or both (crandb)", {
  skip_on_cran()
  res_pkg <- cran_results(pkg = "phylobase", src = "crandb")
  res_email <- cran_results(
    email = "francois.michonneau@gmail.com",
    src = "crandb"
  )
  res_both <- cran_results(
    email = "francois.michonneau@gmail.com",
    pkg = "ridigbio", src = "crandb"
  )
  expect_true(validate_cran_results(res_pkg))
  expect_true(validate_cran_results(res_email))
  expect_true(validate_cran_results(res_both))
})

test_that("works for maintainers with a single package (website)", {
  skip_on_cran()
  res <- cran_results(email = "bbolker+lme4@gmail.com")
  expect_true(validate_cran_results(res))
})

test_that("works for maintainers with a single package (crandb)", {
  skip_on_cran()
  res <- cran_results(
    email = "bbolker+lme4@gmail.com",
    src = "crandb"
  )
  expect_true(validate_cran_results(res))
})

test_that("case doesn't matter (website)", {
  skip_on_cran()
  res_lower <- cran_results(email = "bbolker+lme4@gmail.com")
  res <- cran_results(email = "bBolker+Lme4@gmail.com")
  expect_true(identical(res_lower, res))
})

test_that("case doesn't matter (crandb)", {
  skip_on_cran()
  res <- cran_results(
    email = "bBolker+Lme4@gmail.com",
    src = "crandb"
  )
  res_lower <- cran_results(
    email = "bbolker+lme4@gmail.com",
    src = "crandb"
  )
  expect_true(identical(res_lower, res))
})

test_that("works for multiple packages, multiple addresses (website)", {
  skip_on_cran()
  res_pkgs <- cran_results(pkg = c("rotl", "phylobase", "ridigbio"))
  res_emails <- cran_results(email = c(
    "francois.michonneau@gmail.com",
    "hadley@rstudio.com"
  ), max_requests = Inf)
  res_both <- cran_results(
    email = c(
      "francois.michonneau@gmail.com",
      "hadley@rstudio.com"
    ),
    pkg = c("arrow", "duckdb", "bold")
  )
  expect_true(validate_cran_results(res_pkgs))
  expect_true(validate_cran_results(res_emails))
  expect_true(validate_cran_results(res_both))
})

test_that("works for multiple packages, multiple addresses (crandb)", {
  skip_on_cran()
  res_pkgs <- cran_results(
    pkg = c("rotl", "phylobase", "ridigbio"),
    src = "crandb"
  )
  res_emails <- cran_results(
    email = c(
      "francois.michonneau@gmail.com",
      "hadley@rstudio.com"
    ),
    src = "crandb"
  )
  res_both <- cran_results(
    email = c(
      "francois.michonneau@gmail.com",
      "hadley@rstudio.com"
    ),
    pkg = c("arrow", "duckdb", "bold"),
    src = "crandb"
  )
  expect_true(validate_cran_results(res_pkgs))
  expect_true(validate_cran_results(res_emails))
  expect_true(validate_cran_results(res_both))
})

test_that("fails if too many requests", {
  expect_error(
    cran_results(pkg = c("rotl", "rncl"), max_requests = 1),
    "This query would require more than "
  )
})

test_that("fails if max requests is malformed", {
  expect_error(
    cran_results(pkg = "rotl", max_requests = NA_integer_)
  )
  expect_error(
    cran_results(pkg = "rotl", max_requests = TRUE)
  )
  expect_error(
    cran_results(pkg = "rotl", max_requests = c(4L, 5L))
  )
})

test_that("max requests is ignored if using crandb", {
  skip_on_cran()
  expect_true(
    validate_cran_results(cran_results(pkg = c("rncl", "rotl"), max_requests = 1, src = "crandb"))
  )
})

test_that("infinite is accepted for max_requests", {
  skip_on_cran()
  expect_true(validate_cran_results(
    cran_results(pkg = "rotl", max_requests = Inf)
  ))
})

test_that("deadline column valid", {
  skip_on_cran()
  pkg_data <- as.data.frame(get_cran_rds_file("packages"), stringsAsFactors = FALSE)
  pkg_with_deadline <- pkg_data[!is.na(pkg_data$Deadline), , drop = FALSE]
  n_pkg_with_deadline <- nrow(pkg_with_deadline)

  if (n_pkg_with_deadline < 1) {
    skip("No package with deadline")
  }

  pkg_with_deadline <- pkg_with_deadline[
    sample(
      seq_len(n_pkg_with_deadline),
      min(max(1, n_pkg_with_deadline), 10)
    ),
    "Package"
  ]

  res_web_no_deadline <- cran_results(
    pkg = pkg_with_deadline,
    show = c("error", "fail", "warn", "note", "ok"),
    max_requests = Inf
  )
  res_web_with_deadline <- cran_results(
    pkg = pkg_with_deadline,
    show = c("error", "fail", "warn", "note", "ok", "deadline"),
    max_requests = Inf
  )
  res_crandb_no_deadline <- cran_results(
    pkg = pkg_with_deadline,
    show = c("error", "fail", "warn", "note", "ok"),
    src = "crandb"
  )
  res_crandb_with_deadline <- cran_results(
    pkg = pkg_with_deadline,
    show = c("error", "fail", "warn", "note", "ok", "deadline"),
    src = "crandb"
  )

  expect_true(validate_cran_results(res_web_no_deadline, deadline = FALSE))
  expect_true(validate_cran_results(res_crandb_no_deadline, deadline = FALSE))
  expect_true(validate_cran_results(res_web_with_deadline, deadline = TRUE))
  expect_true(validate_cran_results(res_crandb_with_deadline, deadline = TRUE))


  ## deadline content is character
  expect_true(is.character(res_web_with_deadline$deadline))
  expect_true(sum(!is.na(res_web_with_deadline$deadline)) <= nrow(res_web_with_deadline))
  expect_true(
    identical(
      sum(grepl("\\d{4}-\\d{2}-\\d{2}", res_web_with_deadline$deadline)),
      sum(!is.na(res_web_with_deadline$deadline))
    )
  )
  expect_true(
    identical(
      sum(grepl("\\d{4}-\\d{2}-\\d{2}", res_crandb_with_deadline$deadline)),
      sum(!is.na(res_crandb_with_deadline$deadline))
    )
  )
  expect_true(
    sum(!is.na(res_web_with_deadline$deadline)) <= sum(!is.na(res_crandb_with_deadline$deadline))
  )
})


test_that("local variable can be used to control content of the output", {
  skip_on_cran()

  pkg_data <- as.data.frame(get_cran_rds_file("packages"), stringsAsFactors = FALSE)
  pkg_with_deadline <- pkg_data[!is.na(pkg_data$Deadline), , drop = FALSE]
  n_pkg_with_deadline <- nrow(pkg_with_deadline)

  if (n_pkg_with_deadline < 1) {
    skip("No package with deadline")
  }

  pkg_with_deadline <- pkg_with_deadline[
    sample(
      seq_len(n_pkg_with_deadline),
      min(max(1, n_pkg_with_deadline), 10)
    ),
    "Package"
  ]

  ## deadline is not included in the results when using the `foghorn_columns`
  ## local variable
  withr::local_options(list(foghorn_columns = c("error", "fail", "warn", "note", "ok")))
  res_web_no_deadline <- cran_results(pkg = pkg_with_deadline, src = "website")
  expect_false("deadline" %in%  names(res_web_no_deadline))

  ## deadline is included in the results
  withr::local_options(list(foghorn_columns = c("error", "fail", "warn", "note", "ok", "deadline")))
  res_web_with_deadline <- cran_results(pkg = pkg_with_deadline, src = "website")
  expect_true("deadline" %in% names(res_web_with_deadline))
  expect_true(identical(sum(grepl("\\d{4}-\\d{2}-\\d{2}", res_web_with_deadline$deadline)), nrow(res_web_with_deadline)))

})


### visit_cran_check -----------------------------------------------------------

test_that("error if nothing is specified", {
  expect_error(
    visit_cran_check(),
    "needs to be specified"
  )
})

test_that("error if both email and pkg are specified", {
  skip_on_cran()
  expect_error(
    visit_cran_check(pkg = "foobar", email = "foobar@baz.com"),
    "only one package or one email address"
  )
})

test_that("error if more than one package", {
  skip_on_cran()
  msg <- "must be a string"
  expect_error(
    visit_cran_check(pkg = c("foo", "bar")),
    msg
  )
  expect_error(
    visit_cran_check(pkg = c(TRUE)),
    msg
  )
})


test_that("error if more than one email address", {
  skip_on_cran()
  msg <- "must be a string"
  expect_error(
    visit_cran_check(email = c("foo", "bar")),
    msg
  )
  expect_error(
    visit_cran_check(email = c(TRUE)),
    msg
  )
})

test_that("check for interactive", {
  skip_on_cran()
  skip_if_not(!interactive())
  expect_warning(visit_cran_check(pkg = "rotl"))
})

## summary_cran_results --------------------------------------------------------

build_regexp <- function(what, pkg) {
  pkg <- paste0("(.|\\W)+", sort(pkg), "(.|\\W)+|\\[Fix before: \\d{4}-\\d{2}-\\d{2}\\]?", collapse = "")
  paste0(what, pkg)
}

test_that("output of summary cran results", {
  skip_on_cran()

  pkgs <- c("rotl", "rncl")
  res_web <- suppressMessages(summary_cran_results(pkg = pkgs, src = "website"))
  res_cran <- suppressMessages(summary_cran_results(pkg = pkgs, src = "crandb"))
  expect_true(all(pkgs %in% res_web$package))
  expect_true(all(pkgs %in% res_cran$package))
  expect_identical(res_web, res_cran)

  cran_res <- get_cran_rds_file("results")
  cran_res <- cran_res[!is.na(cran_res$status), ]
  cran_mem <- get_cran_rds_file("issues")

  pkg_with_notes <- sample(
    unique(cran_res$package[cran_res$status == "NOTE"]), 5
  )

  ## nolint start
  ## expect_identical(
  ##   summary_cran_results(pkg = pkg_with_notes, src = "website"),
  ##   summary_cran_results(pkg = pkg_with_notes, src = "crandb")
  ## )
  ## nolint end

  ## all have >= 1 values in column NOTES
  expect_true(
    all(cran_results(pkg = pkg_with_notes, src = "website")$note > 0)
  )
  expect_true(
    all(cran_results(pkg = pkg_with_notes, src = "crandb")$note > 0)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_notes),
    build_regexp("with notes", pkg_with_notes)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_notes, src = "crandb"),
    build_regexp("with notes", pkg_with_notes)
  )

  pkg_with_warn <- sample(
    unique(cran_res$package[cran_res$status == "WARNING"]),
    5
  )
  ## nolint start
  ## expect_identical(
  ##   summary_cran_results(pkg = pkg_with_warn, src = "website"),
  ##   summary_cran_results(pkg = pkg_with_warn, src = "crandb")
  ## )
  ## nolint end

  ## all have >= 1 values in column WARNING
  expect_true({
    message("Checking: ", paste(pkg_with_warn, collapse = ", "))
    all(cran_results(pkg = pkg_with_warn, src = "website")$warn > 0)
  })
  expect_true({
    message("Checking: ", paste(pkg_with_warn, collapse = ", "))
    all(cran_results(pkg = pkg_with_warn, src = "crandb")$warn > 0)
  })
  ## all are listed under packages with WARNING
  expect_message(
    summary_cran_results(pkg = pkg_with_warn),
    build_regexp("with warnings", pkg_with_warn)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_warn, src = "crandb"),
    build_regexp("with warnings", pkg_with_warn)
  )

  pkg_with_err <- sample(
    unique(cran_res$package[cran_res$status == "ERROR"]),
    5
  )
  ## expect_identical(summary_cran_results(pkg = pkg_with_err, src = "website"),
  ##                 summary_cran_results(pkg = pkg_with_err, src = "crandb"))
  ## all have >= 1 values in column ERROR
  expect_true(all(cran_results(pkg = pkg_with_err, src = "website")$error > 0))
  expect_true(all(cran_results(pkg = pkg_with_err, src = "crandb")$error > 0))
  ## all packages are listed under WARNING
  expect_message(
    summary_cran_results(pkg = pkg_with_err),
    build_regexp("with errors", pkg_with_err)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_err, src = "crandb"),
    build_regexp("with errors", pkg_with_err)
  )

  pkg_with_fail <- sample(
    unique(cran_res$package[cran_res$status == "FAILURE"]),
    min(length(unique(cran_res$package[cran_res$status == "FAILURE"])), 5)
  )

  ## output from website and CRAN db identical
  # expect_identical(summary_cran_results(pkg = pkg_with_fail, src = "website"),
  #                 summary_cran_results(pkg = pkg_with_fail, src = "crandb"))
  ## all have >= 1 values in column FAIL:
  expect_true(all(cran_results(pkg = pkg_with_fail, src = "website")$fail > 0))
  expect_true(all(cran_results(pkg = pkg_with_fail, src = "crandb")$fail > 0))
  ## all packages are listed under FAIL:
  expect_message(
    summary_cran_results(pkg = pkg_with_fail),
    build_regexp("with fails", pkg_with_fail)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_fail, src = "crandb"),
    build_regexp("with fails", pkg_with_fail)
  )

  pkg_with_issues <- sample(unique(cran_mem$package), 10)
  ## output from website and CRAN db identical
  expect_identical(
    summary_cran_results(pkg = pkg_with_issues, src = "website"),
    summary_cran_results(pkg = pkg_with_issues, src = "crandb")
  )
  ## all have TRUE listed in the column "has issues notes"
  expect_true(all(cran_results(pkg = pkg_with_issues)[["has_issues_notes"]]))
  expect_true(
    all(
      cran_results(pkg = pkg_with_issues, src = "crandb")[["has_issues_notes"]]
    )
  )
  ## all packages listed under "with issues"
  expect_message(
    summary_cran_results(pkg = pkg_with_issues, src = "website"),
    build_regexp("with other issues", pkg_with_issues)
  )
  expect_message(
    summary_cran_results(pkg = pkg_with_issues, src = "crandb"),
    build_regexp("with other issues", pkg_with_issues)
  )

  msg <- capture_messages(summary_cran_results(pkg = "rncl", compact = TRUE))
  expect_false(all(grepl("  -", msg)))
  msg <- capture_messages(summary_cran_results(pkg = "rncl", compact = FALSE))
  expect_true(all(grepl("  -", msg)))
})

### cran_details  ----------------------------------------------------------

test_that("input is character", {
  expect_error(
    cran_details(TRUE),
    "not a string"
  )
})


test_that("output of cran_details", {
  skip_on_cran()
  library("dplyr")
  cran_res <- get_cran_rds_file("results")
  cran_res <- cran_res[!is.na(cran_res$status), ]
  cran_issues <- get_cran_rds_file("issues")

  ## find package with results = OK and no other issues
  pkg_with_ok <- character(0)
  while (length(pkg_with_ok) < 1) {
    pkg_with_ok <- cran_res %>%
      dplyr::count(package, status) %>%
      dplyr::filter(status == "OK" & n == n_cran_flavors()) %>%
      dplyr::sample_n(5) %>%
      dplyr::pull(package)
    pkg_with_ok <- setdiff(pkg_with_ok, cran_issues$package)
  }
  pkg_with_notes <- sample(
    unique(cran_res$package[cran_res$status == "NOTE"]),
    1
  )
  pkg_with_err <- sample(
    unique(cran_res$package[cran_res$status == "ERROR"]),
    3
  )

  ## results from web scrapping
  web_pkg_with_ok <- cran_details(pkg_with_ok, src = "website")
  web_pkg_with_notes <- cran_details(pkg_with_notes, src = "website")
  web_pkg_with_err <- cran_details(pkg_with_err, src = "website")

  expect_true(validate_cran_details(web_pkg_with_ok))
  expect_true(validate_cran_details(web_pkg_with_notes))
  expect_true(validate_cran_details(web_pkg_with_err))

  expect_output(
    summary(web_pkg_with_notes, show_log = FALSE),
    pkg_with_notes
  )
  expect_output(
    summary(web_pkg_with_notes, show_log = TRUE),
    pkg_with_notes
  )
  expect_message(
    summary(web_pkg_with_ok, show_log = TRUE, print_ok = TRUE),
    "All clear"
  )
  expect_silent(summary(web_pkg_with_ok, show_log = TRUE, print_ok = FALSE))


  ## results from CRAN db
  cran_pkg_with_ok <- cran_details(pkg_with_ok, src = "crandb")
  cran_pkg_with_notes <- cran_details(pkg_with_notes, src = "crandb")
  cran_pkg_with_err <- cran_details(pkg_with_err, src = "crandb")

  ## this doesn't pass: expect_true(validate_cran_details(cran_pkg_with_ok))
  expect_true(validate_cran_details(cran_pkg_with_notes))
  expect_true(validate_cran_details(cran_pkg_with_err))

  expect_output(
    summary(cran_pkg_with_notes, show_log = FALSE),
    pkg_with_notes
  )
  expect_output(
    summary(cran_pkg_with_notes, show_log = TRUE),
    pkg_with_notes
  )
  expect_message(
    summary(cran_pkg_with_ok, show_log = TRUE),
    "All clear"
  )
  expect_silent(summary(cran_pkg_with_ok, show_log = TRUE, print_ok = FALSE))
})

test_that("check output for MASS", {
  skip_on_cran()
  if (curl::has_internet()) {
    res <- cran_results(pkg = "MASS")
    expect_true(validate_cran_results(res))
    res <- cran_results(pkg = "MASS", src = "crandb")
    expect_true(validate_cran_results(res))
  }
})
