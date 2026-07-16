make_pkg_page <- function(...) {
  spans <- vapply(
    c(...),
    function(txt) {
      paste0("<tr><td><span style=\"color:red\">", txt, "</span></td></tr>")
    },
    character(1)
  )
  html <- paste0("<html><body><table>", paste(spans, collapse = ""),
    "</table></body></html>")
  list(xml2::read_html(html))
}

test_that("extract_deadline returns NA when there is no deadline", {
  expect_identical(
    extract_deadline(make_pkg_page("[email to maintainer is undeliverable]")),
    NA_character_
  )
  expect_identical(
    extract_deadline(list(xml2::read_html("<html><body></body></html>"))),
    NA_character_
  )
})

test_that("extract_deadline picks the deadline span", {
  expect_identical(
    extract_deadline(make_pkg_page("[issues need fixing before 2026-07-30]")),
    "2026-07-30"
  )
})

test_that("extract_deadline handles multiple styled spans (#issue)", {
  ## CRAN pages may contain other styled spans (e.g. an undeliverable
  ## email notice) alongside the deadline; extraction must not error.
  expect_identical(
    extract_deadline(make_pkg_page(
      "[email to maintainer is undeliverable]",
      "[issues need fixing before 2026-07-23]"
    )),
    "2026-07-23"
  )
})
