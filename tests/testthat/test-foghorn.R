context("fail for invalid package name/emails")

test_that("invalid package name", {
    expect_error(cran_check_results(package = "foobarfoobar"),
                 "Invalid package name")
})

test_that("invalid email address", {
    expect_error(cran_check_results(email = "foobar"),
                 "Malformed email address")
    expect_error(cran_check_results(email = "foo@foobarbar.rrrr"),
                 "Invalid email address")
})

context("cran check results")

check_cran_check_results <- function(x) {
    length(x) == 6 &&
        inherits(x, "tbl_df") &&
        all(names(x) %in% c("Package", "ERROR", "WARN", "NOTE",  "OK", "has_memtest_notes")) &&
        nrow(x) > 0
}

test_that("at least email or package name specified", {
    expect_error(cran_check_results(), "provide at least one value for")
})

test_that("works with one package, one address, or both", {
    res_pkg <- cran_check_results(package = "phylobase")
    res_email <- cran_check_results(email = "francois.michonneau@gmail.com")
    res_both <- cran_check_results(email = "francois.michonneau@gmail.com",
                                   package = "ridigbio")
    expect_true(check_cran_check_results(res_pkg))
    expect_true(check_cran_check_results(res_email))
    expect_true(check_cran_check_results(res_both))
})

test_that("works for maintainers with a single package", {
    res <- cran_check_results(email = "bbolker+lme4@gmail.com")
    expect_true(check_cran_check_results(res))
})

test_that("works for multiple packages, multiple addresses", {
    res_pkgs <- cran_check_results(package = c("rotl", "phylobase", "ridigbio"))
    res_emails <- cran_check_results(email = c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"))
    res_both <- cran_check_results(email =   c("francois.michonneau@gmail.com",
                                               "hadley@rstudio.com"),
                                   package = c("ridigbio", "mregions", "bold"))
    expect_true(check_cran_check_results(res_pkgs))
    expect_true(check_cran_check_results(res_emails))
    expect_true(check_cran_check_results(res_both))
})


context("visit cran check")
test_that("error if nothing is specified",  {
    expect_error(visit_cran_check(),
                 "needs to be specified")
})

test_that("error if both email and pkg are specified", {
    expect_error(visit_cran_check(pkg = "foobar", email = "foobar@baz.com"),
                 "only one package or one email address")
})

test_that("error if more than one package", {
    msg <- "must be a string"
    expect_error(visit_cran_check(pkg = c("foo", "bar")),
                 msg)
    expect_error(visit_cran_check(pkg = c(TRUE)),
                 msg)
})


test_that("error if more than one email address", {
    msg <- "must be a string"
    expect_error(visit_cran_check(email = c("foo", "bar")),
                 msg)
    expect_error(visit_cran_check(email = c(TRUE)),
                 msg)
})
