.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts_foghorn <- list(
    foghorn_include_deadline = TRUE
  )
  toset <- !(names(opts_foghorn) %in% names(opts))
  if (any(toset)) options(opts_foghorn[toset])

  invisible()
}
