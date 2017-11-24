## 0.x.x

* Add initial support for https://cranchecks.info as a data source (not
  exported/tested at this stage).

* Fix bug #14 reported by @hadley, no issues are now represented as 0 instead of `NA`

* Fix bug that would display a number instead of the package name in some
  situations

* Take into consideration issues other than memtest when parsing the HTML CRAN
  check page.
  
* `check_cran_results` and `show_cran_results` are deprecated.

* Add argument `print_ok` to `summary()` method for `cran_results` that can
  optionally print an "all clear" message when all CRAN checks return "OK" for a
  package.

## v0.4.4

* CRAN has merged memtest notes with a new "other issues" that run valgrind and
  other memory issues on multiple platforms/compilers. Therefore, memtest is
  renamed "other issues".

## v0.4.2

* initial release on CRAN
