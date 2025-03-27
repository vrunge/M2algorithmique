## GPL-3 License
## Copyright (c) 2025 Vincent Runge

#' Heap Sort Algorithm
#'
#' @description
#' Implements the heap sort algorithm, which sorts a numeric vector by first 
#' building a max heap and then repeatedly extracting the largest element 
#' to place it in its correct position in the sorted array.
#'
#' @param v A numeric vector to be sorted.
#' @return A numeric vector, sorted in ascending order.
#' 
#' @details
#' Heap sort works by transforming the input vector into a max heap, a binary 
#' tree where each parent node is greater than or equal to its child nodes. 
#' After constructing the heap, the largest element (the root) is swapped with 
#' the last element, and the heap is rebuilt for the remaining elements. This 
#' process repeats until all elements are sorted.
#'
#' @examples
#' heap_sort(rnorm(100))
#' heap_sort(sample(100))
heap_sort <- function(v)
{
  n <- length(v)

    for(i in (n%/%2):1)
    {
      v <- build_heap(v, i, n)
    }
    for(i in n:2)
    {
      temp <- v[i]
      v[i] <- v[1]
      v[1] <- temp
      v <- build_heap(v, 1, i-1)
    }
  
  return(v)
}


build_heap <- function(heap, i, n)
{
  l <- 2*i
  if(l <= n)
  {
    if((l < n) && (heap[l] < heap[l+1])){l <- l + 1} #choose the right son
    if(heap[i] < heap[l]) #switch the node values
    {
      temp <- heap[i]
      heap[i] <- heap[l]
      heap[l] <- temp
      heap <- build_heap(heap, l, n)
    }
  }
  return(heap)
}

