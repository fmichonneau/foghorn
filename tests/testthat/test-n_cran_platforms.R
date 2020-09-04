cache_file <- file.path(tempdir(), "foghorn-n_cran_flavors.rds")
current_cran_flavors <- 12L

test_that("caching for CRAN flavors", {
  skip_on_cran()
  unlink(cache_file)
  expect_false(file.exists(cache_file))
  expect_identical(n_cran_flavors(), current_cran_flavors)
  expect_true(file.exists(cache_file))
})

test_that("disabling caching for CRAN flavors", {
  skip_on_cran()
  unlink(cache_file)
  expect_false(file.exists(cache_file))
  expect_identical(n_cran_flavors(use_cache = FALSE), current_cran_flavors)
  expect_false(file.exists(cache_file))
})

test_that("specifying force default for CRAN flavors", {
  skip_on_cran()
  unlink(cache_file)
  expect_false(file.exists(cache_file))
  expect_identical(n_cran_flavors(force_default = TRUE, n_flavors = 999L), 999L)
  expect_false(file.exists(cache_file))
})


test_that("assertions for n_cran_flavors", {
  expect_error(n_cran_flavors(use_cache = 123))
  expect_error(n_cran_flavors(use_cache = "123"))
  expect_error(n_cran_flavors(use_cache = logical(0)))
  expect_error(n_cran_flavors(use_cache = c(TRUE, FALSE)))
  expect_error(n_cran_flavors(use_cache = NA))
  expect_error(n_cran_flavors(use_cache = NULL))

  expect_error(n_cran_flavors(force_default = 123))
  expect_error(n_cran_flavors(force_default = "123"))
  expect_error(n_cran_flavors(force_default = logical(0)))
  expect_error(n_cran_flavors(force_default = c(TRUE, FALSE)))
  expect_error(n_cran_flavors(force_default = NA))
  expect_error(n_cran_flavors(force_default = NULL))

  expect_error(n_cran_flavors(n_flavors = 123))
  expect_error(n_cran_flavors(n_flavors = "123"))
  expect_error(n_cran_flavors(n_flavors = integer(0)))
  expect_error(n_cran_flavors(n_flavors = c(1234L, 5678L)))
  expect_error(n_cran_flavors(use_cache = NA_integer_))
  expect_error(n_cran_flavors(use_cache = NULL))
})
