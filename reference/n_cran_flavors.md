# The number of CRAN flavors

The CRAN flavors, the systems on which CRAN tests all packages
regularly, are listed
<https://cran.r-project.org/web/checks/check_flavors.html>. To get the
correct results, foghorn needs to know how many flavors CRAN uses. This
function reads the number of flavors that CRAN currently uses, and
caches it (per session, in the
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) folder). Arguments
control caching, fall back, and default values.

## Usage

``` r
n_cran_flavors(
  use_cache = getOption("foghorn.use_cache", TRUE),
  force_default = getOption("foghorn.force_default", FALSE),
  n_flavors = getOption("foghorn.n_flavors", 12L)
)
```

## Arguments

- use_cache:

  Should the value for the number of flavors be read to/ written from
  the cache? (default: `TRUE`)

- force_default:

  Should the default value be used? (default: `FALSE`). When `TRUE`, the
  number of flavors is read from the Internet.

- n_flavors:

  What is the default number of flavors? (default: `12L`)

## Value

The number of CRAN check flavors (as an integer).

## Details

The default values for the arguments are read from options. Given that
`n_cran_flavors` function is relied on internally to provide accurate
information to the user, using options allows you to control how the
function behaves directly. In general, the default values should not be
changed. They are provided in case you have issues connecting to the web
page listing the number of flavors, or you do not want to use caching.

The options can be set:

- by session, using, for
  instance,`options("foghorn.use_cache" = FALSE)`.

- permanently, by adding `options("foghorn.use_cache" = FALSE)` in your
  `.Rprofile`.

- for a specific call, using the `withr` package:
  `withr::with_options(foghorn.use_cache = FALSE, ...)`.
