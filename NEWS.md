#  foghorn 1.6.0

## New Features

* The output of `cran_results()` and `summary_cran_results()` can now include
  the deadline fixed by CRAN. This deadline is the date by which requested
  changes need to be implemented before the package gets archived. To see any
  deadline that might have been set by CRAN Maintainers, add (`"deadline"`) as
  one of the values to the `show` argument. The value of the `show` argument
  can also be controled by the local option `foghorn_columns`.
* The `cran_results()` function gains the `max_requests` argument that limits
  the number of requests that are performed against the CRAN website in a single
  query. 

## Bug fixes

* Packages that only have "other issues" will no longer show "All clear" in the
  summary output.

## Other changes

* The `{crayon}` package has been replaced with `cli` for styling the output.
* The `{clisymbols}` package has been replaced with `cli` for the symbols
  displayed in the output. (#60, reported by @olivroy)
* When using the CRAN database as a data source, functions will error if the
  email address specified is invalid. This behavior is consistent with what
  happens when using the CRAN website as a data source.

# foghorn 1.5.2

## Other changes

* Address change in formatting of CRAN result page that affected the retrieval
  of the results.
* `cran_results()` will now error if the package fails to retrieve results from
  CRAN. (#53, suggested by @eddelbuettel)

# foghorn 1.5.1

## New Features

* The `cran_incoming()` function does not include the packages found the
  `archive` folder by default. (#49, suggested by @krlmlr)
* The `cran_incoming()` gains a new argument. The results are sorted
  by date (in decreasing order) by default. Using `sort_by_date = FALSE`
  to get the old behavior (#49, suggested by @krlmlr).
  

## Other changes

* The progress bar for larger file downloads from CRAN server has been
  removed.
* The `httr2` package is now used internally to download files and
  data from the CRAN servers.
* The unexported code to use the crancheck API
  (https://cranchecks.info/) has been removed from the code base.
* All support for FTP access has been removed.

# foghorn 1.4.2

## Other changes

* The parsing of incorrectly formatted package names submitted to CRAN's queue
  does not lead `foghorn` to error. The version number for such packages is now
  `NA` instead of `0.0.0`. (#45, reported by @bastistician)
* Rename internal function to reflect it does not use FTP anymore.

# foghorn 1.4.1

## Other changes

* The number of CRAN flavors is now at 14.

# foghorn 1.4.0

## Other Changes

* The inspection of the queue from CRAN incoming is now using HTTPS instead of
  FTP.
* The data in the `size` column for the object returned by `cran_incoming()` is
  now of type character.

# foghorn 1.3.2

## Bug fixes

* The output of `winbuilder_queue()` was always empty following a change to the
  content returned by the Win-builder FTP server (#43, reported by @bbolker).

# foghorn 1.3.1

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

# foghorn 1.2.3

* The number of CRAN check flavors is back to 12.

# foghorn 1.2.2

* The number of CRAN check flavors is now 11.

# foghorn 1.2.1

* The number of CRAN check flavors is back to 12. A new function
  `n_cran_flavors` reads the table on the CRAN website that lists the number of
  flavors, caches it, and returns this number. This is a more robust way to
  ensure that the number of flavors (used in the package) is always accurate.
  There is also the possibility of setting the number of flavors (and disabling
  caching) using options, see the help for the `n_cran_flavors()` function for
  more information.

# foghorn 1.1.5

* internally replaced `as.tibble` with `as_tibble`

# foghorn 1.1.4

* The number of CRAN check flavors is now 13. Code and tests have been adjusted
  to take this change into account.

# foghorn 1.1.3

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


# foghorn 1.1.0

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


# foghorn 1.0.2

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

# foghorn 1.0.1

## New features

* Add `cran_incoming()` to retrieve the list of packages currently in the CRAN
  incoming queue. Feature requested by @krlmlr, #24.
  
* `foghorn` is now compatible with R >= 3.1. Suggested by @jimhester, #26.

# foghorn 1.0.0

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

# foghorn 0.4.4

* CRAN has merged memtest notes with a new "other issues" that run valgrind and
  other memory issues on multiple platforms/compilers. Therefore, memtest is
  renamed "other issues".

# foghorn 0.4.2

* initial release on CRAN
