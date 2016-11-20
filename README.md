
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/fmichonneau/foghorn.svg?branch=master)](https://travis-ci.org/fmichonneau/foghorn) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/fmichonneau/foghorn?branch=master&svg=true)](https://ci.appveyor.com/project/fmichonneau/foghorn) [![Coverage Status](https://img.shields.io/codecov/c/github/fmichonneau/foghorn/master.svg)](https://codecov.io/github/fmichonneau/foghorn?branch=master) ![Work in Progress](https://img.shields.io/badge/status-work%20in%20progress-yellow.svg) [![](http://www.r-pkg.org/badges/version/foghorn)](http://www.r-pkg.org/pkg/foghorn)

foghorn
=======

> **foghorn** *noun* <br> 1. Device used to facilitate navigation in foggy conditions by warning of potential hazards ahead.

`foghorn` makes accessible to the R terminal the results of the CRAN check results for the packages maintained by individuals, or for other package of interests. It provides a graphical summary of the results designed to added to your `.Rprofile` (to check regularly on the status of the published packages), or as a tibble.

As new features are introduced in development versions of R, or new policies are put in place, packages that are not updated frequently may start generating warnings or errors when built by CRAN. `foghorn` brings this information to your terminal automatically so you don't have to check the CRAN check results page regularly.

The package uses [whoami](https://cran.r-project.org/package=whoami) to guess your email address, but it can be specified manually.

Installation
------------

You can install foghorn from github with:

``` r
# install.packages("ghit")
ghit::install_github("fmichonneau/foghorn")
```

Example
-------

``` r
## load the package
library(foghorn)

## Graphical interface
summary_cran_checks(email = "francois.michonneau@gmail.com")
#> ⚠ Package(s) with warnings on CRAN: rotl (1)
#> ★ Package(s) with notes on CRAN: rncl (3)

## Summary as a data frame
cran_check_results(email = "francois.michonneau@gmail.com")
#> # A tibble: 4 × 6
#>     Package ERROR  WARN  NOTE    OK has_memtest_notes
#>       <chr> <int> <int> <int> <int>             <lgl>
#> 1 phylobase    NA    NA    NA    12             FALSE
#> 2  riceware    NA    NA    NA    12             FALSE
#> 3      rncl    NA    NA     3     9             FALSE
#> 4      rotl    NA     1    NA    11             FALSE

## You can also have information just for some packages
summary_cran_checks(email = NULL,  package = c("mregions", "ridigbio"))
#> ✖ Package(s) with errors on CRAN: mregions (1)
cran_check_results(email = NULL,  package = c("mregions", "ridigbio"))
#> # A tibble: 2 × 6
#>    Package ERROR  WARN  NOTE    OK has_memtest_notes
#>      <chr> <int> <int> <int> <int>             <lgl>
#> 1 mregions     1    NA    NA    11             FALSE
#> 2 ridigbio    NA    NA    NA    12             FALSE

## Or both
summary_cran_checks(email = "francois.michonneau@gmail.com",  package = c("mregions", "ridigbio"))
#> ✖ Package(s) with errors on CRAN: mregions (1)
#> ⚠ Package(s) with warnings on CRAN: rotl (1)
#> ★ Package(s) with notes on CRAN: rncl (3)
cran_check_results(email = "francois.michonneau@gmail.com",  package = c("mregions", "ridigbio"))
#> # A tibble: 6 × 6
#>     Package ERROR  WARN  NOTE    OK has_memtest_notes
#>       <chr> <int> <int> <int> <int>             <lgl>
#> 1  mregions     1    NA    NA    11             FALSE
#> 2  ridigbio    NA    NA    NA    12             FALSE
#> 3 phylobase    NA    NA    NA    12             FALSE
#> 4  riceware    NA    NA    NA    12             FALSE
#> 5      rncl    NA    NA     3     9             FALSE
#> 6      rotl    NA     1    NA    11             FALSE

## You can also get the results details of the check results for packages:
summary_cran_results("rotl")
#> ⚠ rotl - WARN: re-building of vignette outputs
#>    ❯ r-devel-windows-ix86+x86_64 
#> 
#>     Error in re-building vignettes:
#>       ...
#>     Quitting from lines 47-58 (meta-analysis.Rmd) 
#>     Error: processing vignette 'meta-analysis.Rmd' failed with diagnostics:
#>     SI number '1' greater than number of detected SIs (0)
#>     Execution halted
#> 
#> ⚠ rotl - WARN: re-building of vignette outputs
#>    ❯ r-patched-solaris-sparc 
#> 
#>     Error in re-building vignettes:
#>       ...
#>     Warning in engine$weave(file, quiet = quiet, encoding = enc) :
#>       Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
#>     Warning in engine$weave(file, quiet = quiet, encoding = enc) :
#>       Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
#>     Warning in engine$weave(file, quiet = quiet, encoding = enc) :
#>       Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
#>     
#>      *** caught segfault ***
#>     address 441ec5f0, cause 'memory not mapped'
#>     
#>     Traceback:
#>      1: .Call("readxl_xls_col_types", PACKAGE = "readxl", path, na, sheet,     nskip, n, has_col_names)
#>      2: xls_col_types(path, sheet, na = na, nskip = skip, has_col_names = has_col_names)
#>      3: read_xls(path, sheet, col_names, col_types, na, skip)
#>      4: read_excel(xl_file)
#>      5: eval(expr, envir, enclos)
#>      6: eval(expr, envir, enclos)
#>      7: withVisible(eval(expr, envir, enclos))
#>      8: withCallingHandlers(withVisible(eval(expr, envir, enclos)), warning = wHandler,     error = eHandler, message = mHandler)
#>      9: handle(ev <- withCallingHandlers(withVisible(eval(expr, envir,     enclos)), warning = wHandler, error = eHandler, message = mHandler))
#>     10: timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,     envir, enclos)), warning = wHandler, error = eHandler, message = mHandler)))
#>     11: evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,     debug = debug, last = i == length(out), use_try = stop_on_error !=         2L, keep_warning = keep_warning, keep_message = keep_message,     output_handler = output_handler, include_timing = include_timing)
#>     12: evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),     keep_message = !isFALSE(options$message), stop_on_error = if (options$error &&         options$include) 0L else 2L, output_handler = knit_handlers(options$render,         options))
#>     13: in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,     keep_warning = !isFALSE(options$warning), keep_message = !isFALSE(options$message),     stop_on_error = if (options$error && options$include) 0L else 2L,     output_handler = knit_handlers(options$render, options)))
#>     14: block_exec(params)
#>     15: call_block(x)
#>     16: process_group.block(group)
#>     17: process_group(group)
#>     18: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),     error = function(e) {        setwd(wd)        cat(res, sep = "\n", file = output %n% "")        message("Quitting from lines ", paste(current_lines(i),             collapse = "-"), " (", knit_concord$get("infile"),             ") ")    })
#>     19: process_file(text, output)
#>     20: knit(input, text = text, envir = envir, encoding = encoding,     quiet = quiet)
#>     21: knit2html(..., force_v1 = TRUE)
#>     22: (if (grepl("\\.[Rr]md$", file)) knit2html_v1 else if (grepl("\\.[Rr]rst$",     file)) knit2pdf else knit)(file, encoding = encoding, quiet = quiet,     envir = globalenv())
#>     23: vweave(...)
#>     24: engine$weave(file, quiet = quiet, encoding = enc)
#>     25: doTryCatch(return(expr), name, parentenv, handler)
#>     26: tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>     27: tryCatchList(expr, classes, parentenv, handlers)
#>     28: tryCatch({    engine$weave(file, quiet = quiet, encoding = enc)    setwd(startdir)    find_vignette_product(name, by = "weave", engine = engine)}, error = function(e) {    stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",         file, conditionMessage(e)), domain = NA, call. = FALSE)})
#>     29: buildVignettes(dir = "/home/ripley/R/packages/tests32/rotl.Rcheck/vign_test/rotl")
#>     An irrecoverable exception occurred. R is aborting now ...
```

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
