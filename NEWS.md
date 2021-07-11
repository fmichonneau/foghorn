# foghorn

# v1.4.2

## Other changes

* The parsing of incorrectly formatted package names submitted to CRAN's queue
  does not lead `foghorn` to error. The version number for such packages is now
  `NA` instead of `0.0.0`. (#45, reported by @bastistician)
* Rename internal function to reflect it does not use FTP anymore.

# v1.4.1

## Other changes

* The number of CRAN flavors is now at 14.

# v1.4.0

## Other Changes

* The inspection of the queue from CRAN incoming is now using HTTPS instead of
  FTP.
* The data in the `size` column for the object returned by `cran_incoming()` is
  now of type character.

# v1.3.2

## Bug fixes

* The output of `winbuilder_queue()` was always empty following a change to the
  content returned by the Win-builder FTP server (#43, reported by @bbolker).

# v1.3.1

## New feature

* Implement scrapping of Win-builder queue (#40 suggested by @krlmlr).

## Other changes

* In `cran_incoming()` and `winbuilder_queue()` the version numbers in the
  tibbles are of class `package_version` (suggested by @krlmlr).
* `cran_incoming()` output includes the size of the tarball archive.
* `cran_incoming()` returns a zero-row tibble instead of `NULL` when the
  inspected folder is empty.
* The argument `progress` was not documented (and not implemented properly) for
  `cran_results()` and `cran_details()`.

# v1.2.3

* The number of CRAN check flavors is back to 12.

# v1.2.2

* The number of CRAN check flavors is now 11.

# v1.2.1

* The number of CRAN check flavors is back to 12. A new function
  `n_cran_flavors` reads the table on the CRAN website that lists the number of
  flavors, caches it, and returns this number. This is a more robust way to
  ensure that the number of flavors (used in the package) is always accurate.
  There is also the possibility of setting the number of flavors (and disabling
  caching) using options, see the help for the `n_cran_flavors()` function for
  more information.

# v1.1.5

* internally replaced `as.tibble` with `as_tibble`

# v1.1.4

* The number of CRAN check flavors is now 13. Code and tests have been adjusted
  to take this change into account.

# v1.1.3

## New feature

* The documentation of `cran_incoming()` is improved (#35 by @bbolker).
* The folder `waiting` in the CRAN submission queue wasn't documented and could
  not be inspected by using the argument `folders` in the function
  `cran_incoming()` (#35 by @bbolker and #38, reported by @HenrikBengtsson).

## Other changes

* The vignette "Quick start" has been renamed "Get started" so it could be more
  visible and easily accessible in the documentation website (#36 reported by
  @maelle).
* The argument `v.names` in the function `stats::reshape` wasn't fully spelled
  (#37 by @jennybc).


# v1.1.0

## New features

* The output of `cran_incoming` now includes the date/time (contribution by
  @bbolker, #30)
* `foghorn` respects the CRAN mirror set by the users instead of using
  `https://cran.r-project.org`.
* When a failure (other than 404) occurs while trying to obtain the data for a
  package, `foghorn` will retry up to three times.


## Bug fixes

* `foghorn` would, in some cases, return that a valid package name published on
  CRAN did not exit (#29, @zkamvar).
* The recent `newbies` folder found on the CRAN FTP incoming server has been
  added to the list of places to check packages in CRAN's submission queue
  (#32).
* When a package only had "additional issues", the summary functions would
  report that everything was clear (#33, @coatless)


# v1.0.2

## New features

* Let users filter the CRAN incoming folder they want to inspect. Feature
  requested by @krlmlr, #28.

## Bug fixes

* Don't display progress bar in non-interactive mode for file download.

## Other changes

* The `noemail` folder has been removed from the CRAN FTP incoming server.
  `cran_incoming()` has been updated to reflect this change.
* `foghorn` has an hex logo. Thanks to Maëlle Salmon for feedback on initial
  design.

# v1.0.1

## New features

* Add `cran_incoming()` to retrieve the list of packages currently in the CRAN
  incoming queue. Feature requested by @krlmlr, #24.
  
* `foghorn` is now compatible with R >= 3.1. Suggested by @jimhester, #26.

# v1.0.0

## API changes

* `check_cran_results()` and `show_cran_results()` are deprecated in favor of
  `cran_results()` and `summary_cran_details()` respectively.

## New features

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

## Bugs fixed

* Fix bug #14 reported by @hadley, no issues are now represented as 0 instead of
  `NA`.

* Fix bug that would display a number instead of the package name in some
  situations.

# v0.4.4

* CRAN has merged memtest notes with a new "other issues" that run valgrind and
  other memory issues on multiple platforms/compilers. Therefore, memtest is
  renamed "other issues".

# v0.4.2

* initial release on CRAN
