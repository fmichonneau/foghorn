# Summary of the CRAN check results

Given the email address of a package maintainer, and/or a vector of
package names, it displays at the console a summary of the check results
run on the CRAN flavors. This function is designed to be included in
your .Rprofile to be run (periodically) at start up.

## Usage

``` r
summary_cran_results(
  email = NULL,
  pkg = NULL,
  compact = FALSE,
  print_ok = TRUE,
  ...
)

# S3 method for class 'cran_results'
summary(object, compact = FALSE, print_ok = TRUE, ...)

show_cran_results(...)
```

## Arguments

- email:

  email address for package maintainers (character vector)

- pkg:

  package names (character vector)

- compact:

  if `TRUE`, all the packages with non-OK results are listed in a single
  line, otherwise they are listed on multiple lines.

- print_ok:

  if `TRUE` the summary method will print a "all clear" message for
  package(s) that have an OK status for all CRAN checks.

- ...:

  additional arguments to control where the data from the check results
  are coming from and how they are downloaded from the CRAN servers (see
  Details section).

- object:

  an object created by `cran_results`

## Value

Prints the packages that return errors, warnings, and notes on the CRAN
flavors. The number in parenthesis after the name of the packages
indicates the number of CRAN flavors that produce these results.

## Examples

``` r
if (FALSE) { # \dontrun{
   summary_cran_results(email = c("user1@company1.com", "user2@company2.com"))
   summary_cran_results(email = "user1@company1.com",
                        pkg = c("pkg1", "pkg2"))
} # }
```
