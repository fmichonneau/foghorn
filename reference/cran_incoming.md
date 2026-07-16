# List packages in CRAN incoming queue.

Check where your package stands in the CRAN incoming queue.

## Usage

``` r
cran_incoming(
  pkg = NULL,
  folders = cran_incoming_folders(),
  sort_by_date = TRUE
)

cran_incoming_folders(include_archive = FALSE)
```

## Arguments

- pkg:

  Optionally provide a vector of package names to limit the results to
  these packages.

- folders:

  Which folders of the CRAN queue do you want to inspect? Default: all
  the non-human folders.

- sort_by_date:

  when `TRUE` (default), the output is sorted in decreasing order
  according to the submission time.

- include_archive:

  when `TRUE`, the function `cran_incoming_folders()` also returns the
  `archive` folder.

## Value

`cran_incoming()` returns `tibble` with the following columns:

- package:

  package name

- version:

  package version

- cran_folder:

  folder where the package was found

- time:

  date/time package was entered in the folder

- size:

  the size of the package tarball

`cran_incoming_folders()` returns a character vector of the names of the
folders used as part of the CRAN submission process, `archive` being
included optionally.

Note that if the package version is not provided, it will appear as `NA`
in the `tibble`.

## Details

When submitting a package to CRAN, it undergoes a series of checks
before it is published and publicly available. `cran_incoming()` allows
you to check the packages that are currently in the queue, and the
folder where they are located. This information could help you track
your package submission. Only the following folders are considered
(approximately in order of the CRAN queue sequence): `newbies`,
`inspect`, `pretest`, `recheck`, `pending`, `waiting`, `publish`,
`special`. The folder `archive` is not inspected by default. The folders
named after the initials of the CRAN volunteers are not inspected.

## Note

The meaning of the package folders is as follows (see Hornik, Ligges and
Zeileis
<https://journal.r-project.org/news/RJ-2018-1-cran/RJ-2018-1-cran.pdf>
and Uwe Ligges mailing list comment
<https://stat.ethz.ch/pipermail/r-package-devel/2019q1/003631.html>):

- newbies:

  for first time submission; package will be manually inspected.

- inspect:

  package is awaiting manual inspection; always happens for first time
  submissions and for packages with problems that are likely to be false
  positives

- pretest:

  a human has triggered a new auto-check of the package

- recheck:

  package has passed checks and is waiting for reverse dependency
  checking

- pending:

  a CRAN team member has to do a closer inspection and needs more time

- waiting:

  CRAN's decision is waiting for a response from the package maintainer,
  e.g. when issues are present that CRAN cannot check for in the
  incoming checks

- publish:

  package is awaiting publication

- special:

  package is undergoing an additional, non-standard check (e.g. `noLD`,
  `valgrind`, `gcc-san`); unlike the other folders, `special` itself
  contains one sub-folder per check, so packages found here are reported
  with a `cran_folder` of the form `special/<check>`, e.g.
  `special/noLD`

- archive:

  package rejected: it does not pass the checks cleanly and the problems
  are unlikely to be false positives

## Disclaimer

The information provided here is only to give you an indication of where
your package stands in the submission process. It can be useful to
confirm that your package has been correctly uploaded to CRAN. Please
consult the [CRAN Repository
Policy](https://cran.r-project.org/web/packages/policies.html) if you
have any questions.

## References

- Hornik, Ligges and Zeileis. "Changes on CRAN: 2017-12-01 to
  2018-06-30", R Journal 10(1), July 2018.
  <https://journal.r-project.org/news/RJ-2018-1-cran/RJ-2018-1-cran.pdf>

- Maëlle Salmon, Locke Data, Stephanie Locke, Mitchell O'Hara-Wild, Hugo
  Gruson. "CRAN incoming dashboard",
  <https://lockedata.github.io/cransays/articles/dashboard.html>

## See also

cran_winbuilder

## Examples
