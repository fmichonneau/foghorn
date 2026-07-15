# foghorn

> **foghorn** *noun*  
> 1. Device used to facilitate navigation in foggy conditions by warning
> of potential hazards ahead.

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

[`summary_cran_results()`](https://fmichonneau.github.io/foghorn/reference/summary_cran_results.md)
provides you with a graphical summary of the results of the CRAN checks.
The number in parenthesis after the name of the package indicates the
number of platforms used by CRAN that produced this result. If CRAN has
set a deadline by which your package needs to be updated before it gets
archived, that date will be shown in the output.

``` r

## Graphical interface
summary_cran_results(email = "francois.michonneau@gmail.com", pkg = "lme4")
#> ✔ All clear for foghorn, riceware, rncl, and rotl!
#> ✖  Package with errors on CRAN: 
#>   - lme4 (9)
#> ★  Package with notes on CRAN: 
#>   - phylobase (2)
#> ◉  Package with other issues on CRAN: 
#>   - lme4
#> ☒  Package with deadlines on CRAN: 
#>   - lme4 [Fix before: 2026-07-19]
```

[`summary_cran_results()`](https://fmichonneau.github.io/foghorn/reference/summary_cran_results.md)
is actually an alias of `summary(cran_results())`, meaning that you can
call
[`cran_results()`](https://fmichonneau.github.io/foghorn/reference/cran_results.md)
directly if you want to easily access the underlying data for the
results of the CRAN checks. These results are stored in a tibble.

``` r

## Results of the checks as a tibble
cran_results(email = "francois.michonneau@gmail.com")
#> # A tibble: 5 × 8
#>   package   error  fail  warn  note    ok deadline has_other_issues
#>   <chr>     <int> <int> <int> <int> <int> <chr>    <lgl>           
#> 1 foghorn       0     0     0     0    13 <NA>     FALSE           
#> 2 phylobase     0     0     0     2    11 <NA>     FALSE           
#> 3 riceware      0     0     0     0    13 <NA>     FALSE           
#> 4 rncl          0     0     0     0    13 <NA>     FALSE           
#> 5 rotl          0     0     0     0    13 <NA>     FALSE
```

In addition of your own packages, you can also check the results for any
other packages that might be of interest to you:

``` r

## either by themselves
summary_cran_results(pkg = c("ggplot2", "dplyr"))
#> ✖  Package with errors on CRAN: 
#>   - dplyr (2)
#> ★  Packages with notes on CRAN: 
#>   - dplyr (4)
#>   - ggplot2 (2)

cran_results(pkg = c("ggplot2", "dplyr"))
#> # A tibble: 2 × 8
#>   package error  fail  warn  note    ok deadline has_other_issues
#>   <chr>   <int> <int> <int> <int> <int> <chr>    <lgl>           
#> 1 dplyr       2     0     0     4     7 <NA>     FALSE           
#> 2 ggplot2     0     0     0     2    11 <NA>     FALSE

## or by combining them with email addresses
summary_cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("arrow", "duckdb")
)
#> ✔ All clear for duckdb, foghorn, riceware, rncl, and rotl!
#> ✖  Package with errors on CRAN: 
#>   - arrow (5)
#> ★  Package with notes on CRAN: 
#>   - phylobase (2)
#> ☒  Package with deadlines on CRAN: 
#>   - arrow [Fix before: 2026-07-25]

cran_results(
  email = "francois.michonneau@gmail.com",
  pkg = c("arrow", "duckdb")
)
#> # A tibble: 7 × 8
#>   package   error  fail  warn  note    ok deadline   has_other_issues
#>   <chr>     <int> <int> <int> <int> <int> <chr>      <lgl>           
#> 1 arrow         5     0     0     0     8 2026-07-25 FALSE           
#> 2 duckdb        0     0     0     0    13 <NA>       FALSE           
#> 3 foghorn       0     0     0     0    13 <NA>       FALSE           
#> 4 phylobase     0     0     0     2    11 <NA>       FALSE           
#> 5 riceware      0     0     0     0    13 <NA>       FALSE           
#> 6 rncl          0     0     0     0    13 <NA>       FALSE           
#> 7 rotl          0     0     0     0    13 <NA>       FALSE
```

You can inspect the logs for the check results using
`summary_cran_details(pkg)` (or `summary(cran_details(pkg))`), while
`visit_cran_check(pkg)` takes you directly to the CRAN webpage.

``` r

(tidyr_checks <- cran_details(pkg = "tidyr"))
#> # A tibble: 2 × 7
#>   package version result      check  flavors n_flavors message                  
#>   <chr>   <chr>   <chr>       <chr>  <chr>       <int> <chr>                    
#> 1 tidyr   "1.3.2" OK          ""     ""             13 ""                       
#> 2 tidyr   ""      other_issue "rchk" <NA>           NA "See: <https://raw.githu…
summary(tidyr_checks)
#> ◉ tidyr - other_issue: rchk
#> 
#> See: <https://raw.githubusercontent.com/kalibera/cran-checks/master/rchk/results/tidyr.out>
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
#>   package error  fail  warn  note    ok deadline has_other_issues
#>   <chr>   <int> <int> <int> <int> <int> <chr>    <lgl>           
#> 1 nlme        0     0     0     0    13 <NA>     FALSE
```

Check out the “Details” section in the help files for more information.

## Feedback? Suggestions?

Feel free to submit feedback and suggestions by [opening an
issue](https://github.com/fmichonneau/foghorn/issues/new) on GitHub.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://fmichonneau.github.io/foghorn/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
