##  GPL-3 License
## Copyright (c) 2020 Vincent Runge

#' Insertion sort algorithm
#'
#' @description Sorting by insertion
#' @param v an unsorted vector of numeric data
#' @return the sorted vector
insertion_sort <- function(v)
{
  for(j in 2:length(v)) 
  {
    key <- v[j] 
    i <- j - 1 
    while(i > 0 && v[i] > key)
    {
      v[i + 1] <- v[i]
      i <- i - 1 
    }
    v[i + 1] <- key
  }
  return(v)
} 

#########################################################
#########################################################

#' Heap sort algorithm
#'
#' @description Sorting by insertion with a heap structure
#' @param v an unsorted vector of numeric data
#' @param type there are two versions for the heap building : a recursive one and a direct one
#' @return the sorted vector
heap_sort <- function(v, type = "notRecursive")
{
  n <- length(v)
  if(type == "notRecursive")
  {
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
  }
  else
  {
    for(i in (n%/%2):1)
    {
      v <- build_heap_recursive(v, i, n)
    }
    for(i in n:2)
    {
      temp <- v[i]
      v[i] <- v[1]
      v[1] <- temp
      v <- build_heap_recursive(v, 1, i-1)
    }
  }
  return(v)
}

####################

build_heap <- function(heap, i, n)
{
  k <- i
  l <- 2*k
  while(l <= n)
  {
    if((l < n) && (heap[l] < heap[l+1])){l <- l + 1} #choose the right son
    if(heap[k] < heap[l]) #switch the node values
    {
      temp <- heap[k]
      heap[k] <- heap[l]
      heap[l] <- temp
      k <- l
      l <- 2*k
    }
    else
    {
      l <- n + 1
    }
  }
  return(heap)
}


####################

build_heap_recursive <- function(heap, i, n)
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
      heap <- build_heap_recursive(heap, l, n)
    }
  }
  return(heap)
}

