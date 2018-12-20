library(testthat)
library(foghorn)

res <- cran_incoming()
stop(str(res))

test_check("foghorn")
