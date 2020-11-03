
[![Build Status](https://travis-ci.com/vrunge/M2algorithmique.svg?branch=main)](https://travis-ci.com/vrunge/M2Algorithmique)

# M2algorithmique Vignette

### Vincent Runge

#### LaMME, Evry Paris-Saclay University

### November 3, 2020

> [Quick Start](#qs)

> [Comparing time complexity of the 4 algorithms](#com)

> [Coming soon: other simulations...](#oth)

<a id="qs"></a>

## Quick Start

The `M2algorithmique` R package is an **example package** developed for students building their own R/Rcpp package as part of the **algorithmic M2 courses in Data Science master's program at Evry Paris-Saclay University**. This package contains two algorithmic strategies (**insertion sort** and **heap sort**) implemented in R and in Rcpp.

Insertion sort is of time complexity ***O*(*n*<sup>2</sup>)** as heap sort is ***O*(*n*log(*n*))** (worst case complexity). We aim at highlighting two important features with this package:

1.  Rcpp algorithms are **much more efficient** than their R counterpart
2.  Time complexities **can be compared to** one another

*All the simulations presented in this README file are available in the `myTests.R` file in the forStudents folder which also contains the Rmd file generating this README.md.*

### Package installation

You first need to install the `devtools` package, it can be done easily from Rstudio. We install the package from Github (remove the \# sign):

``` r
#devtools::install_github("vrunge/M2algorithmique")
library(M2algorithmique)
```

### A first simple test

We simulate simple data as follows, with `v` a vector as size `n` containing all the integers from `1` to `n` (exactly one time) in any order.

``` r
n <- 10
v <- sample(n)
```

We've implemeted 4 algorithms:

-   `insertion_sort`
-   `heap_sort`
-   `insertion_sort_Rcpp`
-   `heap_sort_Rcpp`

They all have a unique argument: the unsorted vector `v`.

``` r
v
```

    ##  [1]  3  7  1  2  4  6 10  5  8  9

``` r
insertion_sort(v)
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10

`insertion_sort(v)` returns the sorted vector from `v`.

<a id="com"></a>

## Comparing time complexity of the 4 algorithms

We run all the following examples at fixed vector length `n = 10000`.

### One simulation

We define a function `one.simu` to simplify the simulation study for time complexity.

``` r
one.simu <- function(n, type = "sample", func = "insertion_sort")
{
  if(type == "sample"){v <- sample(n)}else{v <- n:1}
  if(func == "insertion_sort"){t <- system.time(insertion_sort(v))[[1]]}
  if(func == "heap_sort"){t <- system.time(heap_sort(v))[[1]]} 
  if(func == "insertion_sort_Rcpp"){t <- system.time(insertion_sort_Rcpp(v))[[1]]}
  if(func == "heap_sort_Rcpp"){t <- system.time(heap_sort_Rcpp(v))[[1]]}
  return(t)
}
```

We evaluate the time with a given n over the 4 algorithms. We choose

``` r
n <- 10000
```

and we get:

``` r
one.simu(n, func = "insertion_sort")
```

    ## [1] 3.12

``` r
one.simu(n, func = "heap_sort")
```

    ## [1] 0.238

``` r
one.simu(n, func = "insertion_sort_Rcpp")
```

    ## [1] 0.028

``` r
one.simu(n, func = "heap_sort_Rcpp")
```

    ## [1] 0.002

### Some comparisons

we compare the running time at a given length `n` with repeated executions (`nbSimus` times)

``` r
nbSimus <- 10
time1 <- 0; time2 <- 0; time3 <- 0; time4 <- 0

for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "insertion_sort")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "heap_sort")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "insertion_sort_Rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "heap_sort_Rcpp")}

#gain R -> Rcpp
time1/time3
```

    ## [1] 104.8746

``` r
time2/time4
```

    ## [1] 218.5

``` r
#gain insertion -> heap
time1/time2
```

    ## [1] 9.329519

``` r
time3/time4
```

    ## [1] 19.4375

``` r
#max gain
time1/time4
```

    ## [1] 2038.5

<a id="oth"></a>

## Coming soon: other simulations...

We want to verify the worst case time complexity and/or the average time complexity of these algorithms.
