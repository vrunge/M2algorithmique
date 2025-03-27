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

### SORTING OK ###

test_that("sorting is done well",
          {
            vec <- rchisq(100, df = 3)
            expect_equal(insertion_sort(vec), sort(vec))
          })

test_that("sorting is done well",
          {
            vec <- rnorm(100)
            expect_equal(insertion_sort_Rcpp(vec), sort(vec))
          })

test_that("sorting is done well",
          {
            vec <- rnorm(50)
            expect_equal(heap_sort(vec), sort(vec))
          })

test_that("sorting is done well",
          {
            vec <- sample(500)
            expect_equal(heap_sort(vec), sort(vec))
          })

