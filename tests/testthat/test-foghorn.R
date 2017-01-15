context("check source validity")

test_that("invalid source name",
          expect_error(check_cran_results(pkg = "dplyr", source = "foo")))


context("fail for invalid package name/emails")

test_that("invalid package name", {
    expect_error(check_cran_results(pkg = "foobarfoobar"),
                 "Invalid package name")
})

test_that("invalid email address", {
    expect_error(check_cran_results(email = "foobar"),
                 "Malformed email address")
    expect_error(check_cran_results(email = "foo@foobarbar.rrrr"),
                 "Invalid email address")
})

context("check cran results")

validate_check_cran_results <- function(x) {
    length(x) == 7 &&
        inherits(x, "tbl_df") &&
        all(names(x) %in% c("Package", "ERROR", "FAIL", "WARN",
                            "NOTE",  "OK", "has_memtest_notes")) &&
        nrow(x) > 0
}

test_that("at least email or package name specified (website)", {
    expect_error(check_cran_results(), "provide at least one value for")
})

test_that("at least email or package name specified (crandb)", {
    expect_error(check_cran_results(source = "crandb"), "provide at least one value for")
})

test_that("works with one package, one address, or both (website)", {
    skip_on_cran()
    res_pkg <- check_cran_results(pkg = "phylobase")
    res_email <- check_cran_results(email = "francois.michonneau@gmail.com")
    res_both <- check_cran_results(email = "francois.michonneau@gmail.com",
                                   pkg = "ridigbio")
    expect_true(validate_check_cran_results(res_pkg))
    expect_true(validate_check_cran_results(res_email))
    expect_true(validate_check_cran_results(res_both))
})

test_that("works with one package, one address, or both (crandb)", {
    skip_on_cran()
    res_pkg <- check_cran_results(pkg = "phylobase", source = "crandb")
    res_email <- check_cran_results(email = "francois.michonneau@gmail.com",
                                    source = "crandb")
    res_both <- check_cran_results(email = "francois.michonneau@gmail.com",
                                   pkg = "ridigbio", source = "crandb")
    expect_true(validate_check_cran_results(res_pkg))
    expect_true(validate_check_cran_results(res_email))
    expect_true(validate_check_cran_results(res_both))
})

test_that("works for maintainers with a single package (website)", {
    skip_on_cran()
    res <- check_cran_results(email = "bbolker+lme4@gmail.com")
    expect_true(validate_check_cran_results(res))
})

test_that("works for maintainers with a single package (crandb)", {
    skip_on_cran()
    res <- check_cran_results(email = "bbolker+lme4@gmail.com",
                              source = "crandb")
    expect_true(validate_check_cran_results(res))
})

test_that("case doesn't matter (website)", {
    skip_on_cran()
    res_lower <- check_cran_results(email = "bbolker+lme4@gmail.com")
    res <- check_cran_results(email = "bBolker+Lme4@gmail.com")
    expect_true(identical(res_lower, res))
})

test_that("case doesn't matter (crandb)", {
    skip_on_cran()
    res <- check_cran_results(email = "bBolker+Lme4@gmail.com",
                              source = "crandb")
    res_lower <- check_cran_results(email = "bbolker+lme4@gmail.com",
                              source = "crandb")
    expect_true(identical(res_lower, res))
})

test_that("works for multiple packages, multiple addresses (website)", {
    skip_on_cran()
    res_pkgs <- check_cran_results(pkg = c("rotl", "phylobase", "ridigbio"))
    res_emails <- check_cran_results(email = c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"))
    res_both <- check_cran_results(email =   c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"),
                                   pkg = c("ridigbio", "mregions", "bold"))
    expect_true(validate_check_cran_results(res_pkgs))
    expect_true(validate_check_cran_results(res_emails))
    expect_true(validate_check_cran_results(res_both))
})

test_that("works for multiple packages, multiple addresses (crandb)", {
    skip_on_cran()
    res_pkgs <- check_cran_results(pkg = c("rotl", "phylobase", "ridigbio"),
                                   source = "crandb")
    res_emails <- check_cran_results(email = c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"),
                                     source = "crandb")
    res_both <- check_cran_results(email =   c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"),
                                   pkg = c("ridigbio", "mregions", "bold"),
                                   source = "crandb")
    expect_true(validate_check_cran_results(res_pkgs))
    expect_true(validate_check_cran_results(res_emails))
    expect_true(validate_check_cran_results(res_both))
})

### visit_cran_check -----------------------------------------------------------

context("visit cran check")
test_that("error if nothing is specified",  {
    expect_error(visit_cran_check(),
                 "needs to be specified")
})

test_that("error if both email and pkg are specified", {
    skip_on_cran()
    expect_error(visit_cran_check(pkg = "foobar", email = "foobar@baz.com"),
                 "only one package or one email address")
})

test_that("error if more than one package", {
    msg <- "must be a string"
    skip_on_cran()
    expect_error(visit_cran_check(pkg = c("foo", "bar")),
                 msg)
    expect_error(visit_cran_check(pkg = c(TRUE)),
                 msg)
})


test_that("error if more than one email address", {
    msg <- "must be a string"
    skip_on_cran()
    expect_error(visit_cran_check(email = c("foo", "bar")),
                 msg)
    expect_error(visit_cran_check(email = c(TRUE)),
                 msg)
})

test_that("check for interactive", {
    skip_on_cran()
    skip_if_not(!interactive())
    expect_warning(visit_cran_check(pkg = "rotl"))
})

## summary_cran_results --------------------------------------------------------

context("summary cran results")

test_that("output of summary cran results", {
    skip_on_cran()
    set.seed(1010101)
    pkgs <- c("rotl", "rncl")
    res_web <- suppressMessages(summary_cran_results(pkg = pkgs, source = "website"))
    res_cran <- suppressMessages(summary_cran_results(pkg = pkgs, source = "crandb"))
    expect_true(all(pkgs %in% res_web$Package))
    expect_true(all(pkgs %in% res_cran$Package))
    expect_identical(res_web, res_cran)

    cran_res <- get_cran_rds_file("results")
    cran_mem <- get_cran_rds_file("memtest")

    pkg_with_warn <- sample(cran_res$Package[cran_res$Status == "WARN"], 3)
    expect_identical(summary_cran_results(pkg = pkg_with_warn, source = "website"),
                     summary_cran_results(pkg = pkg_with_warn, source = "crandb"))
    expect_message(summary_cran_results(pkg = pkg_with_warn),
                   "with warnings")
    expect_message(summary_cran_results(pkg = pkg_with_warn,
                                        source = "crandb"),
                   "with warnings")

    pkg_with_warn <- sample(cran_res$Package[cran_res$Status == "NOTE"], 3)
    expect_identical(summary_cran_results(pkg = pkg_with_warn, source = "website"),
                     summary_cran_results(pkg = pkg_with_warn, source = "crandb"))
    expect_message(summary_cran_results(pkg = pkg_with_warn),
                   "with notes")
    expect_message(summary_cran_results(pkg = pkg_with_warn,
                                        source = "crandb"),
                   "with notes")

    pkg_with_warn <- sample(cran_res$Package[cran_res$Status == "ERROR"], 3)
    expect_identical(summary_cran_results(pkg = pkg_with_warn, source = "website"),
                     summary_cran_results(pkg = pkg_with_warn, source = "crandb"))
    expect_message(summary_cran_results(pkg = pkg_with_warn),
                   "with errors")
    expect_message(summary_cran_results(pkg = pkg_with_warn,
                                        source = "crandb"),
                   "with errors")

    pkg_with_warn <- sample(cran_res$Package[cran_res$Status == "FAIL"], 3)
    expect_identical(summary_cran_results(pkg = pkg_with_warn, source = "website"),
                     summary_cran_results(pkg = pkg_with_warn, source = "crandb"))
    expect_message(summary_cran_results(pkg = pkg_with_warn),
                   "with fails")
    expect_message(summary_cran_results(pkg = pkg_with_warn,
                                        source = "crandb"),
                   "with fails")

    pkg_with_memtest <- sample(names(cran_mem), 3)
    expect_message(summary_cran_results(pkg = pkg_with_memtest, source = "website"),
                   "with memtest")
    expect_message(summary_cran_results(pkg = pkg_with_memtest, source = "crandb"),
                   "with memtest")

    msg <- capture_messages(summary_cran_results(pkg = "rncl", compact = TRUE))
    expect_false(all(grepl("  -", msg)))
    msg <- capture_messages(summary_cran_results(pkg = "rncl", compact = FALSE))
    expect_true(all(grepl("  -", msg)))
})

### show_cran_results ----------------------------------------------------------

context("show cran results")

test_that("only one package", {
    expect_error(show_cran_results(c("pkg1", "pkg2")),
                 "not a string")
    expect_error(show_cran_results(TRUE),
                 "not a string")
})


test_that("output of show cran results", {
    skip_on_cran()
    expect_output(show_cran_results("rotl", show_log = FALSE),
                  "rotl")
})
