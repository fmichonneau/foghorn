## 0.x.x

* Add support for https://cranchecks.info

* Fix bug #14 reported by @hadley, no issues are now represented as 0 instead of `NA`

* Fix bug that would display a number instead of the package name in some
  situations

* Take into consideration issues other than memtest when parsing the HTML CRAN
  check page.

## v0.4.4

* CRAN has merged memtest notes with a new "other issues" that run valgrind and
  other memory issues on multiple platforms/compilers. Therefore, memtest is
  renamed "other issues".

## v0.4.2

* initial release on CRAN
