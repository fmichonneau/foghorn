# Table of the CRAN check results

Make a table that summarizes the results of the CRAN checks for a set of
packages specified by a maintainer or by names.

## Usage

``` r
cran_results(
  email = NULL,
  pkg = NULL,
  show = getOption("foghorn_columns", c("error", "fail", "warn", "note", "ok",
    "deadline")),
  src = c("website", "crandb"),
  max_requests = 50,
  ...
)
```

## Arguments

- email:

  email address for package maintainers (character vector)

- pkg:

  package names (character vector)

- show:

  columns of the data frame to show (all are shown by default). See
  'Details' for more information.

- src:

  if `"website"` the data is scrapped from the CRAN website, if
  `"crandb"` the data is downloaded from a RDS file hosted on the CRAN
  servers (which is used to generate the information found on the CRAN
  website).

- max_requests:

  maximum number of requests allowed to be performed, ignored when using
  `src = "crandb"`. Use `Inf` to skip this check. (See Details).

- ...:

  additional arguments to control where the data from the check results
  are coming from and how they are downloaded from the CRAN servers (see
  Details section).

## Value

a data frame that tabulates the number of CRAN flavors that return
errors, warnings, notes, or OK for the packages. See 'Details' section
for more information about the `Deadline` column.

## Details

Given the email address of a package maintainer, and/or a vector of
package names, returns a tibble that allows you to detect potential
issues with your packages on CRAN.

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

## Examples

``` r
  if (FALSE) { # \dontrun{
    cran_results(pkg="MASS")
  } # }
```
