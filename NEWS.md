## v1.0.1

## New features

* Add `cran_incoming()` to retrieve the list of packages currently in the CRAN
  incoming queue. Feature requested by @krlmlr, #24.
  
* `foghorn` is now compatible with R >= 3.1. Suggested by @jimhester, #26.

## v1.0.0

### API changes

* `check_cran_results()` and `show_cran_results()` are deprecated in favor of
  `cran_results()` and `summary_cran_details()` respectively.

### New features

* Add initial support for https://cranchecks.info as a data source (not
  exported/tested at this stage).

* Take into consideration issues other than memtest when parsing the HTML CRAN
  check page.

* Add argument `print_ok` to `summary()` method for `cran_results` that can
  optionally print an "all clear" message when all CRAN checks return "OK" for a
  package.

* A progress bar is displayed when the RDS files are being downloaded from CRAN
  if the [progress package](https://github.com/r-lib/progress) is installed
  (#17).
  
* The version number of the packages are displayed in `cran_details()`.

* All the functions return tibbles, with column names in lower case.

### Bugs fixed

* Fix bug #14 reported by @hadley, no issues are now represented as 0 instead of
  `NA`.

* Fix bug that would display a number instead of the package name in some
  situations.

## v0.4.4

* CRAN has merged memtest notes with a new "other issues" that run valgrind and
  other memory issues on multiple platforms/compilers. Therefore, memtest is
  renamed "other issues".

## v0.4.2

* initial release on CRAN
