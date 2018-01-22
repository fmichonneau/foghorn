context("Test `cran_incoming`")

test_that("Documentation is correct", {
    skip_on_cran()
    ## make sure all folders investigated are correct
    expect_silent(res <- cran_incoming())
    expect_true(all(names(res) %in% c("package", "version", "cran_folder")))
    expect_equal(ncol(res), 3L)
    expect_true(nrow(res) > 1)
})

test_that("only character strings are provided",  {
    expect_error(cran_incoming(pkg = c(NA, TRUE)), "character vector")
    expect_error(cran_incoming(pkg = c("foo", NA)), "character vector")
    expect_error(cran_incoming(pkg = 1234L), "character vector")
})

test_that("filtering works", {
    skip_on_cran()
    res <- cran_incoming()
    pkg_test <- sample(res$package, 5L)
    res_test <- cran_incoming(pkg = pkg_test)
    expect_true(nrow(res_test) > 5L)
})
