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
        all(names(x) %in% c("Package", "ERROR", "FAIL", "WARN", "NOTE",  "OK", "has_memtest_notes")) &&
        nrow(x) > 0
}

test_that("at least email or package name specified", {
    expect_error(check_cran_results(), "provide at least one value for")
})

test_that("works with one package, one address, or both", {
    skip_on_cran()
    res_pkg <- check_cran_results(pkg = "phylobase")
    res_email <- check_cran_results(email = "francois.michonneau@gmail.com")
    res_both <- check_cran_results(email = "francois.michonneau@gmail.com",
                                   pkg = "ridigbio")
    expect_true(validate_check_cran_results(res_pkg))
    expect_true(validate_check_cran_results(res_email))
    expect_true(validate_check_cran_results(res_both))
})

test_that("works for maintainers with a single package", {
    skip_on_cran()
    res <- check_cran_results(email = "bbolker+lme4@gmail.com")
    expect_true(validate_check_cran_results(res))
})

test_that("works for multiple packages, multiple addresses", {
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
