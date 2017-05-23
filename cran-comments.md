
### v0.4.4

This submission is in response to Kurt Hornik email, and addresses the reworking
of the memtest output into a more general "other issue" RDS file.


## Test environments

- local Ubuntu 17.04, R 3.4.0
- Ubuntu 12.04 (travis-ci), R 3.4.0
- Windows from R-Hub (R 3.4.0), and R Under development (2017-05-20 r72713) from
  winbuilder
- local Debian, using R Under development (unstable) (2017-05-20 r72713)

## R CMD check results

- There were no ERRORs or WARNINGs

- On winbuilder I saw a WARNING related to the mismatch between the Rcpp and R
  versions against which dplyr was compiled which isn't relevant to my package
  per se.
