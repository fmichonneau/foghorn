context("test winbuilder_queue")

test_that("test format and class for winbuilder", {
  skip_on_cran()

  res <- winbuilder_queue()

  expect_true(inherits(res, "fh_winbuild_q"))
  expect_identical(
    lapply(res, class),
    list(
      package = "character",
      version = c("package_version", "numeric_version"),
      folder = "character",
      time = c("POSIXct", "POSIXt"),
      size = "integer"
    )
  )
})

test_that("folder filtering works", {
  skip_on_cran()

  expect_error(winbuilder_queue(folders = "foo"))
  res <- winbuilder_queue(folders = "R-devel")

  if (nrow(res) < 1) {
    skip("nothing in the winbuilder queue at the time of the check.")
  }
  all(res$folder == "R-devel")
})

test_that("package filtering works", {
  skip_on_cran()
  res <- winbuilder_queue()

  if (nrow(res) < 1) {
    skip("nothing in the winbuilder queue at the time of the check.")
  }

  pkg <- sample(res$package, 1)
  res_pkg <- winbuilder_queue(pkg = pkg)
  expect_true(nrow(res_pkg) <= nrow(pkg))
})
