library(testthat)
library(M2algorithmique)


### RETURNS ###

test_that("return is a vector",
          {
            expect_equal(is.vector(insertion_sort(rnorm(10))), TRUE)
          })
test_that("return is a vector",
          {
            expect_equal(is.vector(insertion_sort_Rcpp(rnorm(10))), TRUE)
          })
test_that("return is a vector",
          {
            expect_equal(is.vector(heap_sort(rnorm(10))), TRUE)
          })
test_that("return is a vector",
          {
            expect_equal(is.vector(heap_sort_Rcpp(rnorm(10))), TRUE)
          })

### ERRORS ###

test_that("error is not a numeric vector",
          {
            expect_error(insertion_sort(sample(LETTERS)))
          })
