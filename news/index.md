# Changelog

## foghorn 1.6.2

### New Features

- [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  now includes packages that are being checked in `special/`
  subdirectories of the CRAN incoming queue (e.g. `special/gcc-ASAN`) by
  default ([\#69](https://github.com/fmichonneau/foghorn/issues/69),
  [@bbolker](https://github.com/bbolker)).

## foghorn 1.6.1

CRAN release: 2025-07-19

### Bug fixes

- Some CRAN mirrors not hosted at the root of the domain name, and
  others that do not support the canonical URL schema were failing
  ([\#63](https://github.com/fmichonneau/foghorn/issues/63), reported
  and fixed by [@louisaslett](https://github.com/louisaslett)).
- Some packages with a deadline but no issues with their CRAN checks
  were not being reported correctly in the output of
  [`summary_cran_results()`](https://fmichonneau.github.io/foghorn/reference/summary_cran_results.md)
  (in part [\#67](https://github.com/fmichonneau/foghorn/issues/67),
  [@trevorld](https://github.com/trevorld)).

## foghorn 1.6.0

CRAN release: 2024-07-02

### New Features

- The output of
  [`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
  and
  [`summary_cran_results()`](https://fmichonneau.github.io/foghorn/reference/summary_cran_results.md)
  can now include the deadline fixed by CRAN. This deadline is the date
  by which requested changes need to be implemented before the package
  gets archived. To see any deadline that might have been set by CRAN
  Maintainers, add (`"deadline"`) as one of the values to the `show`
  argument. The value of the `show` argument can also be controled by
  the local option `foghorn_columns`.
- The
  [`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
  function gains the `max_requests` argument that limits the number of
  requests that are performed against the CRAN website in a single
  query.

### Bug fixes

- Packages that only have “other issues” will no longer show “All clear”
  in the summary output.

### Other changes

- The [crayon](https://r-lib.github.io/crayon/) package has been
  replaced with `cli` for styling the output.
- The [clisymbols](https://github.com/gaborcsardi/clisymbols) package
  has been replaced with `cli` for the symbols displayed in the output.
  ([\#60](https://github.com/fmichonneau/foghorn/issues/60), reported by
  [@olivroy](https://github.com/olivroy))
- When using the CRAN database as a data source, functions will error if
  the email address specified is invalid. This behavior is consistent
  with what happens when using the CRAN website as a data source.

## foghorn 1.5.2

CRAN release: 2024-02-02

### Other changes

- Address change in formatting of CRAN result page that affected the
  retrieval of the results.
- [`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
  will now error if the package fails to retrieve results from CRAN.
  ([\#53](https://github.com/fmichonneau/foghorn/issues/53), suggested
  by [@eddelbuettel](https://github.com/eddelbuettel))

## foghorn 1.5.1

CRAN release: 2022-10-24

### New Features

- The
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  function does not include the packages found the `archive` folder by
  default. ([\#49](https://github.com/fmichonneau/foghorn/issues/49),
  suggested by [@krlmlr](https://github.com/krlmlr))
- The
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  gains a new argument. The results are sorted by date (in decreasing
  order) by default. Using `sort_by_date = FALSE` to get the old
  behavior ([\#49](https://github.com/fmichonneau/foghorn/issues/49),
  suggested by [@krlmlr](https://github.com/krlmlr)).

### Other changes

- The progress bar for larger file downloads from CRAN server has been
  removed.
- The `httr2` package is now used internally to download files and data
  from the CRAN servers.
- The unexported code to use the crancheck API
  (<https://cranchecks.info/>) has been removed from the code base.
- All support for FTP access has been removed.

## foghorn 1.4.2

CRAN release: 2021-07-11

### Other changes

- The parsing of incorrectly formatted package names submitted to CRAN’s
  queue does not lead `foghorn` to error. The version number for such
  packages is now `NA` instead of `0.0.0`.
  ([\#45](https://github.com/fmichonneau/foghorn/issues/45), reported by
  [@bastistician](https://github.com/bastistician))
- Rename internal function to reflect it does not use FTP anymore.

## foghorn 1.4.1

CRAN release: 2021-05-31

### Other changes

- The number of CRAN flavors is now at 14.

## foghorn 1.4.0

CRAN release: 2021-05-05

### Other Changes

- The inspection of the queue from CRAN incoming is now using HTTPS
  instead of FTP.
- The data in the `size` column for the object returned by
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  is now of type character.

## foghorn 1.3.2

CRAN release: 2020-12-17

### Bug fixes

- The output of
  [`winbuilder_queue()`](https://fmichonneau.github.io/foghorn/reference/winbuilder_queue.md)
  was always empty following a change to the content returned by the
  Win-builder FTP server
  ([\#43](https://github.com/fmichonneau/foghorn/issues/43), reported by
  [@bbolker](https://github.com/bbolker)).

## foghorn 1.3.1

CRAN release: 2020-09-08

### New feature

- Implement scrapping of Win-builder queue
  ([\#40](https://github.com/fmichonneau/foghorn/issues/40) suggested by
  [@krlmlr](https://github.com/krlmlr)).

### Other changes

- In
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  and
  [`winbuilder_queue()`](https://fmichonneau.github.io/foghorn/reference/winbuilder_queue.md)
  the version numbers in the tibbles are of class `package_version`
  (suggested by [@krlmlr](https://github.com/krlmlr)).
- [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  output includes the size of the tarball archive.
- [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  returns a zero-row tibble instead of `NULL` when the inspected folder
  is empty.
- The argument `progress` was not documented (and not implemented
  properly) for
  [`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
  and
  [`cran_details()`](https://fmichonneau.github.io/foghorn/reference/cran_details.md).

## foghorn 1.2.3

CRAN release: 2020-07-30

- The number of CRAN check flavors is back to 12.

## foghorn 1.2.2

CRAN release: 2020-07-22

- The number of CRAN check flavors is now 11.

## foghorn 1.2.1

CRAN release: 2020-05-05

- The number of CRAN check flavors is back to 12. A new function
  `n_cran_flavors` reads the table on the CRAN website that lists the
  number of flavors, caches it, and returns this number. This is a more
  robust way to ensure that the number of flavors (used in the package)
  is always accurate. There is also the possibility of setting the
  number of flavors (and disabling caching) using options, see the help
  for the
  [`n_cran_flavors()`](https://fmichonneau.github.io/foghorn/reference/n_cran_flavors.md)
  function for more information.

## foghorn 1.1.5

CRAN release: 2020-04-07

- internally replaced `as.tibble` with `as_tibble`

## foghorn 1.1.4

CRAN release: 2019-11-28

- The number of CRAN check flavors is now 13. Code and tests have been
  adjusted to take this change into account.

## foghorn 1.1.3

CRAN release: 2019-11-10

### New feature

- The documentation of
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  is improved ([\#35](https://github.com/fmichonneau/foghorn/issues/35)
  by [@bbolker](https://github.com/bbolker)).
- The folder `waiting` in the CRAN submission queue wasn’t documented
  and could not be inspected by using the argument `folders` in the
  function
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  ([\#35](https://github.com/fmichonneau/foghorn/issues/35) by
  [@bbolker](https://github.com/bbolker) and
  [\#38](https://github.com/fmichonneau/foghorn/issues/38), reported by
  [@HenrikBengtsson](https://github.com/HenrikBengtsson)).

### Other changes

- The vignette “Quick start” has been renamed “Get started” so it could
  be more visible and easily accessible in the documentation website
  ([\#36](https://github.com/fmichonneau/foghorn/issues/36) reported by
  [@maelle](https://github.com/maelle)).
- The argument `v.names` in the function
  [`stats::reshape`](https://rdrr.io/r/stats/reshape.html) wasn’t fully
  spelled ([\#37](https://github.com/fmichonneau/foghorn/issues/37) by
  [@jennybc](https://github.com/jennybc)).

## foghorn 1.1.0

CRAN release: 2019-02-17

### New features

- The output of `cran_incoming` now includes the date/time (contribution
  by [@bbolker](https://github.com/bbolker),
  [\#30](https://github.com/fmichonneau/foghorn/issues/30))
- `foghorn` respects the CRAN mirror set by the users instead of using
  `https://cran.r-project.org`.
- When a failure (other than 404) occurs while trying to obtain the data
  for a package, `foghorn` will retry up to three times.

### Bug fixes

- `foghorn` would, in some cases, return that a valid package name
  published on CRAN did not exit
  ([\#29](https://github.com/fmichonneau/foghorn/issues/29),
  [@zkamvar](https://github.com/zkamvar)).
- The recent `newbies` folder found on the CRAN FTP incoming server has
  been added to the list of places to check packages in CRAN’s
  submission queue
  ([\#32](https://github.com/fmichonneau/foghorn/issues/32)).
- When a package only had “additional issues”, the summary functions
  would report that everything was clear
  ([\#33](https://github.com/fmichonneau/foghorn/issues/33),
  [@coatless](https://github.com/coatless))

## foghorn 1.0.2

CRAN release: 2018-04-14

### New features

- Let users filter the CRAN incoming folder they want to inspect.
  Feature requested by [@krlmlr](https://github.com/krlmlr),
  [\#28](https://github.com/fmichonneau/foghorn/issues/28).

### Bug fixes

- Don’t display progress bar in non-interactive mode for file download.

### Other changes

- The `noemail` folder has been removed from the CRAN FTP incoming
  server.
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  has been updated to reflect this change.
- `foghorn` has an hex logo. Thanks to Maëlle Salmon for feedback on
  initial design.

## foghorn 1.0.1

CRAN release: 2018-02-28

### New features

- Add
  [`cran_incoming()`](https://fmichonneau.github.io/foghorn/reference/cran_incoming.md)
  to retrieve the list of packages currently in the CRAN incoming queue.
  Feature requested by [@krlmlr](https://github.com/krlmlr),
  [\#24](https://github.com/fmichonneau/foghorn/issues/24).

- `foghorn` is now compatible with R \>= 3.1. Suggested by
  [@jimhester](https://github.com/jimhester),
  [\#26](https://github.com/fmichonneau/foghorn/issues/26).

## foghorn 1.0.0

CRAN release: 2017-12-05

### API changes

- [`check_cran_results()`](https://fmichonneau.github.io/foghorn/reference/check_cran_results.md)
  and
  [`show_cran_results()`](https://fmichonneau.github.io/foghorn/reference/summary_cran_results.md)
  are deprecated in favor of
  [`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
  and
  [`summary_cran_details()`](https://fmichonneau.github.io/foghorn/reference/cran_details.md)
  respectively.

### New features

- Add initial support for <https://cranchecks.info> as a data source
  (not exported/tested at this stage).

- Take into consideration issues other than memtest when parsing the
  HTML CRAN check page.

- Add argument `print_ok` to
  [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `cran_results` that can optionally print an “all clear” message when
  all CRAN checks return “OK” for a package.

- A progress bar is displayed when the RDS files are being downloaded
  from CRAN if the [progress package](https://github.com/r-lib/progress)
  is installed
  ([\#17](https://github.com/fmichonneau/foghorn/issues/17)).

- The version number of the packages are displayed in
  [`cran_details()`](https://fmichonneau.github.io/foghorn/reference/cran_details.md).

- All the functions return tibbles, with column names in lower case.

### Bugs fixed

- Fix bug [\#14](https://github.com/fmichonneau/foghorn/issues/14)
  reported by [@hadley](https://github.com/hadley), no issues are now
  represented as 0 instead of `NA`.

- Fix bug that would display a number instead of the package name in
  some situations.

## foghorn 0.4.4

CRAN release: 2017-05-23

- CRAN has merged memtest notes with a new “other issues” that run
  valgrind and other memory issues on multiple platforms/compilers.
  Therefore, memtest is renamed “other issues”.

## foghorn 0.4.2

CRAN release: 2017-02-08

- initial release on CRAN
