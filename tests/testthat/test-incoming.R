context("Test `cran_incoming`")

skip_on_linux_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS_OS_NAME"), "linux")) {
    return(invisible(TRUE))
  }
  skip("On Travis Linux where the FTP connection doesn't work")
}


test_that("Documentation is correct", {
  skip_on_cran()
  skip_on_linux_travis()

  ## make sure all folders investigated are correct
  expect_silent(res <- cran_incoming())
  expect_true(all(names(res) %in% c("package", "version", "cran_folder", "time")))
  expect_equal(ncol(res), 4L)
  expect_true(nrow(res) > 1)
})

test_that("only character strings are provided", {
  expect_error(cran_incoming(pkg = c(NA, TRUE)), "character vector")
  expect_error(cran_incoming(pkg = c("foo", NA)), "character vector")
  expect_error(cran_incoming(pkg = 1234L), "character vector")
})

test_that("filtering works", {
  skip_on_cran()
  skip_on_linux_travis()
  res <- cran_incoming()
  pkg_test <- sample(res$package, 5L)
  res_test <- cran_incoming(pkg = pkg_test)
  expect_true(nrow(res_test) >= 5L)
})

test_that("specifying folders works", {
  skip_on_cran()
  skip_on_linux_travis()
  res <- cran_incoming(folders = "archive")
  expect_true(nrow(res) > 1 && all(res$cran_folder == "archive"))
  res2 <- cran_incoming(folders = c("inspect", "pending"))
  expect_true(nrow(res2) > 1 && all(res2$cran_folder %in% c("inspect", "pending")))
})
