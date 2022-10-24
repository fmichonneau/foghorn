make_vignettes <- function() {
  vignettes <- list.files("vignettes", pattern = ".Rmd.orig", full.names = TRUE)
  purrr::walk2(
    vignettes,
    file.path("vignettes", gsub("\\.orig", "", basename(vignettes))),
    function(x, y) {
      knitr::knit(x, output = y)
    }
  )
}
