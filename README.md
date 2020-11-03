# M2algorithmique Vignette
### Vincent Runge
#### LaMME, Evry Paris-Saclay University
### November 3, 2020

> [Quick Start](#qs)

> [A few examples](#ex)

<a id="qs"></a>

## Quick Start

The `M2algorithmique` R package is an example package developed for students building their own R/Rcpp package as part of the algorithmic M2 courses. This package contains two algorithmic strategies (insertion sort and heap sort) implemented in R and in Rcpp.

Insertion sort is of time complexity $O(n^2)$ as heap sort is $O(n\log(n))$ (worst case complexity). We highlight two important features with this package:

1. Rcpp algorithms are much efficient than R counterpart
2. Time complexities can be evaluated and compared

### Package installation

We install the package from Github:

```r
devtools::install_github("vrunge/M2algorithmique")
library(M2algorithmique)
```

### Simple data simulation

We simulate simple data as follows

```r
n <- 1000
v <- sample(n)
```

`v` is a vector as size `n` containing all the integers from `1` to `n` (exactly one time) in any order.

We've implemeted 4 algorithms: 

- `insertion_sort` 
- `heap_sort` 
- `insertion_sort_Rcpp` 
- `heap_sort_Rcpp` 



<a id="ex"></a>

## Some examples

### 