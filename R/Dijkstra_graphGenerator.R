##  GPL-3 License
## Copyright (c) 2025 Vincent Runge

#' Generate a Random Weighted Graph
#'
#' This function generates a random weighted graph with `n` vertices and `m` edges,
#' where `m` is determined based on the probability `p` of an edge existing 
#' between any two vertices.
#'
#' @param n Integer. The number of vertices in the graph.
#' @param p Numeric. The probability of an edge existing between two vertices.
#' @return A data frame with three columns: 
#'  \itemize{
#'    \item{"v1"}: The first vertex of the edge.
#'    \item{"v2"}: The second vertex of the edge.
#'    \item{"cost"}: A random weight assigned to the edge (sampled from a normal distribution).
#'    }
#' @examples
#' generate_random_graph(10, 0.5)
#'
#' @export
generate_random_graph <- function(n, p)
{
  m <- floor(p*n*(n-1)/2)
  pairs <- expand.grid(a = 1:n, b = 1:n)
  path <- pairs[pairs$a+1 == pairs$b, ]

  pairs <- pairs[(pairs$a != pairs$b) & (pairs$a+1 != pairs$b), ]  
  if(m > n)
  {
    edges <- pairs[sample(nrow(pairs), size = m-n+1),]
  }
  
  df <- data.frame(rbind(edges,path), abs(rnorm(m)))
  colnames(df) <- c("v1", "v2", "cost")
  rownames(df) <- NULL
  return(df)
}

