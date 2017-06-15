
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/fmichonneau/foghorn.svg?branch=master)](https://travis-ci.org/fmichonneau/foghorn) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/fmichonneau/foghorn?branch=master&svg=true)](https://ci.appveyor.com/project/fmichonneau/foghorn) [![Coverage Status](https://img.shields.io/codecov/c/github/fmichonneau/foghorn/master.svg)](https://codecov.io/github/fmichonneau/foghorn?branch=master) [![](http://www.r-pkg.org/badges/version/foghorn)](http://www.r-pkg.org/pkg/foghorn) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/foghorn)](http://www.r-pkg.org/pkg/foghorn)

foghorn
=======

> **foghorn** *noun* <br> 1. Device used to facilitate navigation in foggy conditions by warning of potential hazards ahead.

`foghorn` makes accessible to the R terminal the results of the CRAN check results for packages maintained by individuals, or for other package of interests. It provides a graphical summary of the results designed to added to your `.Rprofile` (to check regularly on the status of the published packages), or as a tibble.

As new features are introduced in development versions of R, or new policies are put in place, packages that are not updated frequently may start generating warnings or errors when checked on CRAN's infrastructure. `foghorn` brings this information to your terminal so you don't have to leave the comfort of your terminal to know the results of the CRAN checks for your packages.

Installation
------------

You can install foghorn from github with:

``` r
# install.packages("ghit")
ghit::install_github("fmichonneau/foghorn")
```

Demonstration
-------------

``` r
## load the package
library(foghorn)
```

`foghorn` provides a graphical summary for CRAN check results for the packages maintained by individuals (the number is parentheses indicates the number of R flavors used by CRAN that generate notes, warnings, errors):

``` r
## Graphical interface
summary_cran_results(email = "francois.michonneau@gmail.com")
#> ⚠  Packages with warnings on CRAN: 
#>   - foghorn (1)
#>   - rncl (3)
#>   - rotl (3)
#> ★  Package with notes on CRAN: 
#>   - rncl (6)
```

The information can also be summarized as a table:

``` r
## Summary as a tibble
check_cran_results(email = "francois.michonneau@gmail.com")
#> # A tibble: 5 x 7
#>     Package ERROR  FAIL  WARN  NOTE    OK has_other_issues
#>       <chr> <int> <int> <int> <int> <int>            <lgl>
#> 1   foghorn    NA    NA     1    NA    12            FALSE
#> 2 phylobase    NA    NA    NA    NA    13            FALSE
#> 3  riceware    NA    NA    NA    NA    13            FALSE
#> 4      rncl    NA    NA     3     6     4            FALSE
#> 5      rotl    NA    NA     3    NA    10            FALSE
```

In addition of your own packages, you can also check the results for other packages that might be of interest to you:

``` r
## either by themselves
summary_cran_results(pkg = c("ggplot2", "dplyr"))
#> ✖  Packages with errors on CRAN: 
#>   - dplyr (5)
#>   - ggplot2 (2)
#> ★  Packages with notes on CRAN: 
#>   - dplyr (5)
#>   - ggplot2 (4)
#> ◉  Package with other issues on CRAN: 
#>   - dplyr
check_cran_results(pkg = c("ggplot2", "dplyr"))
#> # A tibble: 2 x 7
#>   Package ERROR  FAIL  WARN  NOTE    OK has_other_issues
#>     <chr> <int> <int> <int> <int> <int>            <lgl>
#> 1   dplyr     5    NA    NA     5     3             TRUE
#> 2 ggplot2     2    NA    NA     4     7            FALSE

## or by combining them with email addresses
summary_cran_results(email = "francois.michonneau@gmail.com",
                     pkg = c("mregions", "ridigbio"))
#> ⚠  Packages with warnings on CRAN: 
#>   - foghorn (1)
#>   - rncl (3)
#>   - rotl (3)
#> ★  Packages with notes on CRAN: 
#>   - mregions (2)
#>   - rncl (6)
check_cran_results(email = "francois.michonneau@gmail.com",
                   pkg = c("mregions", "ridigbio"))
#> # A tibble: 7 x 7
#>     Package ERROR  FAIL  WARN  NOTE    OK has_other_issues
#>       <chr> <int> <int> <int> <int> <int>            <lgl>
#> 1  mregions    NA    NA    NA     2    11            FALSE
#> 2  ridigbio    NA    NA    NA    NA    13            FALSE
#> 3   foghorn    NA    NA     1    NA    12            FALSE
#> 4 phylobase    NA    NA    NA    NA    13            FALSE
#> 5  riceware    NA    NA    NA    NA    13            FALSE
#> 6      rncl    NA    NA     3     6     4            FALSE
#> 7      rotl    NA    NA     3    NA    10            FALSE
```

You can also inspect the logs for the check results using `show_cran_results(pkg)`, while `visit_cran_check(pkg)` takes you to the CRAN webpage.

``` r
show_cran_results(pkg = "tidyr")
#> ★ tidyr - NOTE: data for non-ASCII characters
#>    ❯ r-devel-linux-x86_64-fedora-clang 
#>    ❯ r-devel-linux-x86_64-fedora-gcc 
#>    ❯ r-patched-solaris-sparc 
#>    ❯ r-patched-solaris-x86 
#>    ❯ r-release-osx-x86_64 
#>    ❯ r-oldrel-osx-x86_64 
#> 
#>       Note: found 23 marked UTF-8 strings
```

Where does it get the data?
---------------------------

The data from the CRAN check results used by this package are either scrapped from the CRAN web pages (default), or are from the CRAN database. The first option is faster if you want to check regularly a few packages. However, if you are doing statistics on a large number of packages, using the CRAN database is recommended. To use the CRAN database, add `src = "crandb"` in your function calls:

``` r
check_cran_results(pkg = "nlme", src = "crandb")
#> # A tibble: 1 x 7
#>   Package ERROR  FAIL  WARN  NOTE    OK has_other_issues
#>     <chr> <int> <int> <int> <int> <int>            <lgl>
#> 1    nlme     1    NA    NA    NA    12            FALSE
```

Check out the "Details" section in the help files for more information.

Feedback? Suggestions?
----------------------

Feel free to submit feedback and suggestions by [opening an issue](https://github.com/fmichonneau/foghorn/issues/new) on GitHub.

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
