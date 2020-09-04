context("Test `cran_incoming`")

test_that("Documentation is correct", {
  skip_on_cran()
  skip_on_travis()

  ## make sure all folders investigated are correct
  expect_silent(res <- cran_incoming())
  expect_true(all(names(res) %in% c(
    "package", "version", "cran_folder", "time", "size"
  )))
  expect_equal(ncol(res), 5L)
  expect_true(nrow(res) > 1)
  expect_identical(
    lapply(res, class),
    list(
      package = "character",
      version = c("package_version", "numeric_version"),
      cran_folder = "character",
      time = c("POSIXct", "POSIXt"),
      size = "integer"
    )
  )
})

test_that("only character strings are provided", {
  expect_error(cran_incoming(pkg = c(NA, TRUE)), "character vector")
  expect_error(cran_incoming(pkg = c("foo", NA)), "character vector")
  expect_error(cran_incoming(pkg = 1234L), "character vector")
})

test_that("filtering works", {
  skip_on_cran()
  skip_on_travis()
  res <- cran_incoming()
  pkg_test <- sample(res$package, 5L)
  res_test <- cran_incoming(pkg = pkg_test)
  expect_true(nrow(res_test) >= 5L)
})

test_that("specifying folders works", {
  skip_on_cran()
  skip_on_travis()
  res <- cran_incoming(folders = "archive")
  expect_true(nrow(res) > 1 && all(res$cran_folder == "archive"))
  res2 <- cran_incoming(folders = c("inspect", "pending"))
  expect_true(nrow(res2) > 1 && all(res2$cran_folder %in% c("inspect", "pending")))
})
