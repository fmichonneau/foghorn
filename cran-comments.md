## Re-submission

Initial submission got the following on CRAN:

> * checking files in ‘vignettes’ ... WARNING
> Files in the 'vignettes' directory newer than all files in 'inst/doc':
>   ‘quick_start.Rmd’

The vignette was re-generated to remove the WARNING.

## Test environments

- local Ubuntu 16.10, R 3.3.2
- Ubuntu 12.04 (travis-ci), R 3.3.2
- Windows with win-builder (R 3.3.2 and R Under development (2016-11-18 r71668)
- local Debian, using R Under development (unstable) (2016-11-13 r71655)

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 1 NOTE. The URL will be functional once the package is published on
  CRAN.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Francois Michonneau <francois.michonneau@gmail.com>'

New submission

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Francois Michonneau

Possibly mis-spelled words in DESCRIPTION:
  CRAN (2:19, 7:18)

Found the following (possibly) invalid URLs:
  URL: http://www.r-pkg.org/pkg/foghorn
    From: README.md
    Status: 404
    Message: Not Found
```
