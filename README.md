
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

You can install the development version of **`foghorn`** from GitHub
with:

``` r
pak::pkg_install("fmichonneau/foghorn")
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
this result. If CRAN has set a deadline by which your package needs to
be updated before it gets archived, that date will be shown in the
output.

``` r
## Graphical interface
summary_cran_results(email = "francois.michonneau@gmail.com", pkg = "lme4")
#> ✔ All clear for foghorn, phylobase, and riceware!
#> ★  Packages with notes on CRAN: 
#>   - lme4 (8) [Fix before: 2024-06-17]
#>   - rncl (12)
#>   - rotl (10)
#> ◉  Package with other issues on CRAN: 
#>   - lme4 [Fix before: 2024-06-17]
```

`summary_cran_results()` is actually an alias of
`summary(cran_results())`, meaning that you can call `cran_results()`
directly if you want to easily access the underlying data for the
results of the CRAN checks. These results are stored in a tibble.

``` r
## Results of the checks as a tibble
cran_results(email = "francois.michonneau@gmail.com")
#> # A tibble: 5 × 8
#>   package   error  fail  warn  note    ok has_other_issues fix_before
#>   <chr>     <int> <int> <int> <int> <int> <lgl>            <chr>     
#> 1 foghorn       0     0     0     0    13 FALSE            <NA>      
#> 2 phylobase     0     0     0     0    13 FALSE            <NA>      
#> 3 riceware      0     0     0     0    13 FALSE            <NA>      
#> 4 rncl          0     0     0    12     1 FALSE            <NA>      
#> 5 rotl          0     0     0    10     3 FALSE            <NA>
```

In addition of your own packages, you can also check the results for any
other packages that might be of interest to you:

``` r
## either by themselves
summary_cran_results(pkg = c("ggplot2", "dplyr"))
#> ★  Packages with notes on CRAN: 
#>   - dplyr (5)
#>   - ggplot2 (8)
```

``` r

cran_results(pkg = c("ggplot2", "dplyr"))
#> # A tibble: 2 × 8
#>   package error  fail  warn  note    ok has_other_issues fix_before
#>   <chr>   <int> <int> <int> <int> <int> <lgl>            <chr>     
#> 1 dplyr       0     0     0     5     8 FALSE            <NA>      
#> 2 ggplot2     0     0     0     8     5 FALSE            <NA>
```

``` r

## or by combining them with email addresses
summary_cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("arrow", "duckdb")
)
#> ✔ All clear for foghorn, phylobase, and riceware!
#> ⚠  Package with warnings on CRAN: 
#>   - duckdb (1)
#> ★  Packages with notes on CRAN: 
#>   - arrow (11)
#>   - duckdb (11)
#>   - rncl (12)
#>   - rotl (10)
#> ◉  Package with other issues on CRAN: 
#>   - duckdb
```

``` r

cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("arrow", "duckdb")
)
#> # A tibble: 7 × 8
#>   package   error  fail  warn  note    ok has_other_issues fix_before
#>   <chr>     <int> <int> <int> <int> <int> <lgl>            <chr>     
#> 1 arrow         0     0     0    11     2 FALSE            <NA>      
#> 2 duckdb        0     0     1    11     1 TRUE             <NA>      
#> 3 foghorn       0     0     0     0    13 FALSE            <NA>      
#> 4 phylobase     0     0     0     0    13 FALSE            <NA>      
#> 5 riceware      0     0     0     0    13 FALSE            <NA>      
#> 6 rncl          0     0     0    12     1 FALSE            <NA>      
#> 7 rotl          0     0     0    10     3 FALSE            <NA>
```

You can inspect the logs for the check results using
`summary_cran_details(pkg)` (or `summary(cran_details(pkg))`), while
`visit_cran_check(pkg)` takes you directly to the CRAN webpage.

``` r
(tidyr_checks <- cran_details(pkg = "tidyr"))
#> # A tibble: 4 × 7
#>   package version result check                         flavors n_flavors message
#>   <chr>   <chr>   <chr>  <chr>                         <chr>       <dbl> <chr>  
#> 1 tidyr   1.3.1   NOTE   compiled code                 r-deve…         4 "     …
#> 2 tidyr   1.3.1   NOTE   data for non-ASCII characters r-deve…         2 "     …
#> 3 tidyr   1.3.1   NOTE   compiled code                 r-deve…         2 "     …
#> 4 tidyr   1.3.1   ERROR  re-building of vignette outp… r-rele…         2 "     …
```

``` r
summary(tidyr_checks)
#> ★ tidyr - note: compiled code
#>    ❯ r-devel-linux-x86_64-debian-clang 
#>    ❯ r-devel-linux-x86_64-debian-gcc 
#>    ❯ r-devel-linux-x86_64-fedora-clang 
#>    ❯ r-devel-linux-x86_64-fedora-gcc 
#> 
#>      File ‘tidyr/libs/tidyr.so’:
#>      Found non-API calls to R: ‘SETLENGTH’, ‘SET_GROWABLE_BIT’,
#>      ‘SET_TRUELENGTH’
#>      
#>      Compiled code should not call non-API entry points in R.
#>      
#>      See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
#> 
#> ★ tidyr - note: data for non-ASCII characters
#>    ❯ r-devel-linux-x86_64-fedora-clang 
#>    ❯ r-devel-linux-x86_64-fedora-gcc 
#> 
#>      Note: found 24 marked UTF-8 strings
#> 
#> ★ tidyr - note: compiled code
#>    ❯ r-devel-windows-x86_64 
#> 
#>      File 'tidyr/libs/x64/tidyr.dll':
#>      Found non-API calls to R: 'SETLENGTH', 'SET_GROWABLE_BIT',
#>      'SET_TRUELENGTH'
#>      
#>      Compiled code should not call non-API entry points in R.
#>      
#>      See 'Writing portable packages' in the 'Writing R Extensions' manual.
#> 
#> ✖ tidyr - error: re-building of vignette outputs
#>    ❯ r-release-macos-arm64 
#> 
#>      Error(s) in re-building vignettes:
#>      --- re-building ‘in-packages.Rmd’ using rmarkdown
#>      --- finished re-building ‘in-packages.Rmd’
#>      
#>      --- re-building ‘nest.Rmd’ using rmarkdown
#>      --- finished re-building ‘nest.Rmd’
#>      
#>      --- re-building ‘pivot.Rmd’ using rmarkdown
#>      
#>      Quitting from lines 44-47 [setup] (pivot.Rmd)
#>      Error: processing vignette 'pivot.Rmd' failed with diagnostics:
#>      there is no package called 'readr'
#>      --- failed re-building ‘pivot.Rmd’
#>      
#>      --- re-building ‘programming.Rmd’ using rmarkdown
#>      --- finished re-building ‘programming.Rmd’
#>      
#>      --- re-building ‘rectangle.Rmd’ using rmarkdown
#>      --- finished re-building ‘rectangle.Rmd’
#>      
#>      --- re-building ‘tidy-data.Rmd’ using rmarkdown
#>      --- finished re-building ‘tidy-data.Rmd’
#>      
#>      SUMMARY: processing the following file failed:
#>      ‘pivot.Rmd’
#>      
#>      Error: Vignette re-building failed.
#>      Execution halted
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
#> # A tibble: 1 × 8
#>   package error  fail  warn  note    ok has_other_issues fix_before
#>   <chr>   <int> <int> <int> <int> <int> <lgl>            <chr>     
#> 1 nlme        0     0     0     0    13 FALSE            <NA>
```

Check out the “Details” section in the help files for more information.

## Feedback? Suggestions?

Feel free to submit feedback and suggestions by [opening an
issue](https://github.com/fmichonneau/foghorn/issues/new) on GitHub.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://fmichonneau.github.io/foghorn/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
