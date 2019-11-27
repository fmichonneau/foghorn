context("check that the number of CRAN platforms match the global variable")

validate_n_flavors <- function() {
  cran_flavors <- try(
    xml2::read_html("https://cran.rstudio.org/web/checks/check_flavors.html"),
    silent = TRUE
  )
  if (inherits(cran_flavors, "try-error")) {
    warning("Can't connect to check number of flavors. Using hardcoded value.")
    return(13L)
  }

  cran_flvr_tbl <- cran_flavors %>%
    rvest::html_node("table") %>%
    rvest::html_table()

  nrow(cran_flvr_tbl)
}

test_that("foghorn uses the accurate number of CRAN flavors", {
  skip_on_cran()
  expect_true(identical(validate_n_flavors(), 13L))
})
