context("check that the number of CRAN platforms match the global variable")

test_that("n_cran_platforms matches number listed on their website", {
    skip_on_cran()
    cran_platforms_page <- xml2::read_html("https://cran.r-project.org/web/checks/check_flavors.html")
    cran_platforms_table <- rvest::html_table(cran_platforms_page)[[1]]
    expect_true(nrow(cran_platforms_table) == n_cran_platforms)
})
