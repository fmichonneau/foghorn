
<!-- README.md is generated from README.Rmd. Please edit that file -->

![R-CMD-check](https://github.com/fmichonneau/foghorn/workflows/R-CMD-check/badge.svg)
[![Coverage
Status](https://img.shields.io/codecov/c/github/fmichonneau/foghorn/master.svg)](https://app.codecov.io/github/fmichonneau/foghorn?branch=master)
[![CRAN
version](https://www.r-pkg.org/badges/version/foghorn)](https://www.r-pkg.org/pkg/foghorn)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/foghorn)](https://www.r-pkg.org/pkg/foghorn)

# foghorn <img src="man/figures/logo.png" align="right" />

> **foghorn** *noun* <br> 1. Device used to facilitate navigation in
> foggy conditions by warning of potential hazards ahead.

`foghorn` makes accessible to the R terminal the CRAN check results for
packages maintained by individuals, or for other packages of interest.
The results are presented as a colored summary, or as a tibble. The
function that generates the summary is designed to be called from your
`.Rprofile` so you can check on the status of the packages regularly.

As new features are introduced in development versions of R, or new
policies are put in place, packages that are not updated frequently may
start generating warnings or errors when checked on CRAN’s
infrastructure. `foghorn` brings this information to your terminal so
you don’t have to leave the comfort of your R session to know the
results of the CRAN checks for your packages.

## Installation

You can install the development version of **`foghorn`** from github
with:

``` r
remotes::install_github("fmichonneau/foghorn")
```

or the stable version from CRAN:

``` r
install.packages("foghorn")
```

## Demonstration

``` r
## load the package
library(foghorn)
```

`foghorn` provides the results of the package CRAN checks. As a
maintainer, you can easily get the results of the checks for your
packages by using the email address included in the `DESCRIPTION` file
of your packages.

`summary_cran_results()` provides you with a graphical summary of the
results of the CRAN checks. The number in parenthesis after the name of
the package indicates the number of platforms used by CRAN that produced
this result.

``` r
## Graphical interface
summary_cran_results(email = "francois.michonneau@gmail.com")
#> ✔ All clear for riceware!
#> ★  Packages with notes on CRAN: 
#>   - foghorn (2)
#>   - phylobase (6)
#>   - rncl (5)
#>   - rotl (2)
```

`summary_cran_results()` is actually an alias of
`summary(cran_results())`, meaning that you can call `cran_results()`
directly if you want to easily access the underlying data for the
results of the CRAN checks. These results are stored in a tibble.

``` r
## Results of the checks as a tibble
cran_results(email = "francois.michonneau@gmail.com")
#> # A tibble: 5 × 7
#>   package   error  fail  warn  note    ok has_other_issues
#>   <chr>     <int> <int> <int> <int> <int> <lgl>           
#> 1 foghorn       0     0     0     2    10 FALSE           
#> 2 phylobase     0     0     0     6     6 FALSE           
#> 3 riceware      0     0     0     0    12 FALSE           
#> 4 rncl          0     0     0     5     7 FALSE           
#> 5 rotl          0     0     0     2    10 FALSE
```

In addition of your own packages, you can also check the results for any
other packages that might be of interest to you:

``` r
## either by themselves
summary_cran_results(pkg = c("ggplot2", "dplyr"))
#> ★  Packages with notes on CRAN: 
#>   - dplyr (2)
#>   - ggplot2 (7)

cran_results(pkg = c("ggplot2", "dplyr"))
#> # A tibble: 2 × 7
#>   package error  fail  warn  note    ok has_other_issues
#>   <chr>   <int> <int> <int> <int> <int> <lgl>           
#> 1 dplyr       0     0     0     2    10 FALSE           
#> 2 ggplot2     0     0     0     7     5 FALSE

## or by combining them with email addresses
summary_cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("mregions", "ridigbio")
)
#> ✔ All clear for riceware, ridigbio!
#> ★  Packages with notes on CRAN: 
#>   - foghorn (2)
#>   - mregions (6)
#>   - phylobase (6)
#>   - rncl (5)
#>   - rotl (2)

cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("mregions", "ridigbio")
)
#> # A tibble: 7 × 7
#>   package   error  fail  warn  note    ok has_other_issues
#>   <chr>     <int> <int> <int> <int> <int> <lgl>           
#> 1 foghorn       0     0     0     2    10 FALSE           
#> 2 mregions      0     0     0     6     6 FALSE           
#> 3 phylobase     0     0     0     6     6 FALSE           
#> 4 riceware      0     0     0     0    12 FALSE           
#> 5 ridigbio      0     0     0     0    12 FALSE           
#> 6 rncl          0     0     0     5     7 FALSE           
#> 7 rotl          0     0     0     2    10 FALSE
```

You can inspect the logs for the check results using
`summary_cran_details(pkg)` (or `summary(cran_details(pkg))`), while
`visit_cran_check(pkg)` takes you directly to the CRAN webpage.

``` r
(tidyr_checks <- cran_details(pkg = "tidyr"))
#> # A tibble: 2 × 7
#>   package version result check                         flavors n_flavors message
#>   <chr>   <chr>   <chr>  <chr>                         <chr>       <dbl> <chr>  
#> 1 tidyr   1.3.1   NOTE   data for non-ASCII characters r-deve…         2 "     …
#> 2 tidyr   1.3.0   NOTE   C++ specification             r-patc…         2 "     …
summary(tidyr_checks)
#> ★ tidyr - note: data for non-ASCII characters
#>    ❯ r-devel-linux-x86_64-fedora-clang 
#>    ❯ r-devel-linux-x86_64-fedora-gcc 
#> 
#>      Note: found 24 marked UTF-8 strings
#> 
#> ★ tidyr - note: C++ specification
#>    ❯ r-patched-linux-x86_64 
#> 
#>      Specified C++11: please drop specification unless essential
```

## Where does the data come from?

The data from the check results used by this package are either scraped
from the CRAN web pages (default), or are from the CRAN database (that
CRAN uses to build the webpages). The first option is faster if you want
to regularly check a few packages. However, if you are doing statistics
on a large number of packages, using the CRAN database is recommended
(it’s about 20Mb of data). To use the CRAN database, add
`src = "crandb"` in your function calls:

``` r
cran_results(pkg = "nlme", src = "crandb", progress = FALSE)
#> # A tibble: 1 × 7
#>   package error  fail  warn  note    ok has_other_issues
#>   <chr>   <int> <int> <int> <int> <int> <lgl>           
#> 1 nlme        0     0     0     0    12 FALSE
```

Check out the “Details” section in the help files for more information.

## Feedback? Suggestions?

Feel free to submit feedback and suggestions by [opening an
issue](https://github.com/fmichonneau/foghorn/issues/new) on GitHub.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://fmichonneau.github.io/foghorn/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
