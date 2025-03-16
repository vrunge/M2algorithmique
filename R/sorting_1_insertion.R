##  GPL-3 License
##  Copyright (c) 2025 Vincent Runge

#' Insertion Sort Algorithm
#'
#' @description Sorts a numeric vector using the insertion sort algorithm, excluding NA values.
#' @param v A numeric vector to be sorted.
#' @return A sorted numeric vector with NA values excluded.
#' @examples
#' insertion_sort(rnorm(100))
#' insertion_sort(sample(c(NA, 1:100)))
insertion_sort <- function(v)
{
  if (!is.numeric(v)) stop("Input must be a numeric vector")
  if (length(v) < 2) return(v)  # Already sorted if empty or one element
  v <- v[!is.na(v)]   # Exclude NA values
  
  ### main loop
  for (j in 2:length(v))
  {
    key <- v[j]
    i <- j - 1
    while (i > 0 && v[i] > key)
    {
      v[i + 1] <- v[i]
      i <- i - 1
    }
    v[i + 1] <- key
  }
  return(v)
}
