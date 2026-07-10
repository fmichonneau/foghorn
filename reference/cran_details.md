# Get details about the CRAN check results for packages

Given the names of packages published on CRAN, return the output of
checks that return notes, warnings or errors.

## Usage

``` r
cran_details(pkg, src = c("website", "crandb"), ...)

# S3 method for class 'cran_details'
summary(object, show_log = TRUE, print_ok = TRUE, ...)

summary_cran_details(
  pkg,
  src = c("website", "crandb"),
  show_log = TRUE,
  print_ok = TRUE,
  ...
)
```

## Arguments

- pkg:

  character vector of the names for the packages on CRAN

- src:

  if `"website"` the data is scrapped from the CRAN website, if
  `"crandb"` the data is downloaded from a RDS file hosted on the CRAN
  servers (which is used to generate the information found on the CRAN
  website).

- ...:

  additional arguments to control where the data from the check results
  are coming from and how they are downloaded from the CRAN servers (see
  Details section).

- object:

  an object created by `cran_details`

- show_log:

  Should the messages of the “Check Details” be printed? (logical)

- print_ok:

  if `TRUE` the summary method will print a "all clear" message for
  package(s) that have an OK status for all CRAN checks.

## Value

a `tibble` listing the names of the packages that have non- OK check
results, the nature of the result (`WARN`, `ERROR`, `FAIL`, `NOTE`, or
other issues).

## Details

Where does the data come from?

The data comes from the CRAN servers. They generate RDS files that
contains information regarding the results of the checks for all the
packages, and all the flavors. This data is then used to generate the
web pages.

`foghorn` provides access to either of these data sources. If you choose
`src = "website"` the data is scrapped from the CRAN website. If you
only need to check a few packages, this is a good option. If you choose
`src = "crandb"` the RDS files (about 20Mb) are downloaded first from
the CRAN servers.

The option `max_requests` can be used to limit how many pages will be
scrapped from the CRAN website. The default is set to 50, use `Inf` to
ignore this check. Consider using `src = "crandb"` if you need to get
data from many packages or maintainers. Note that this an approximation
of the number of requests that will be performed and there will be
situations where more requests than this limit will be performed to
retrieve the results.

The `deadline` column contains the date set by CRAN to fix issues with
the CRAN checks before the package gets archived. If the existence of a
deadline has been checked but no date has been set by the CRAN
Maintainers, the `deadline` column for the package will be set to `""`.
If there is a deadline, the content will be a date stored as
`character`.

The value of the argument `show` can be set using the local option
`foghorn_columns`.

When choosing `src = "crandb"` you can also specify the following
options:

- `dest`: a folder where to store the RDS files
  ([`tempdir()`](https://rdrr.io/r/base/tempfile.html) by default).

- `protocol`: either `https` or `http`.

- `overwrite`: when `FALSE` (default), if the file exists in `dest` then
  it will not be downloaded again. When `TRUE` the file gets downloaded
  every time it's needed.

## See also

Note that the `tools` package contains unexported functions that can be
used to extract summary information from the check results. Specifically
`tools:::sumarize_CRAN_check_status` is similar to `show_cran_results`.
