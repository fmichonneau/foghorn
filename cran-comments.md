## Re-submissions

### 2017-02-06 (v0.4.2, current)

As suggested by Uwe Ligges, 2 tests (that will not be skipped on CRAN) and one
functional example have been added. Both test for the presence of an active
internet connection. They use functions that will fail if the data format
changes.

### 2017-01-31

The examples are wrapped in \dontrun{} because they require an active internet
connection to run.

### 2016-01-16

Following feedback from Kurt Hornik at the time of initial submission (end of
November), I included the possibility of using the RDS files hosted by CRAN as a
data source for the results, in addition of the original approach scrapping the
content of the CRAN webpages.

I also included a note in the documentation indicating that the "tools" package
included (unexported) functions to summarize information about the CRAN check
results.



## Test environments

- local Ubuntu 16.10, R 3.3.2
- Ubuntu 12.04 (travis-ci), R 3.3.2
- Windows with win-builder (R 3.3.2, and R Under development 2017-01-13 r71966)
- local Debian, using R Under development (unstable) (2017-02-05 r72121)

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 1 NOTE. The URL will be functional once the package is published on
  CRAN.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Francois Michonneau <francois.michonneau@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  CRAN (2:19, 7:18)

Found the following (possibly) invalid URLs:
  URL: http://www.r-pkg.org/pkg/foghorn (moved to https://www.r-pkg.org:443/pkg/foghorn)
    From: README.md
    Status: 404
    Message: Not Found
```
