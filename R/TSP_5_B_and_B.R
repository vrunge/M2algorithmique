##  GPL-3 License
## Copyright (c) 2025 Vincent Runge

#' Branch and Bound Algorithm for Solving the Traveling Salesman Problem (TSP)
#'
#' @description
#' This function implements the \bold{Branch and Bound} algorithm to solve the \bold{Traveling Salesman Problem (TSP)}. 
#' It explores all possible tours but efficiently prunes branches that cannot lead to a better solution 
#' than the best one found so far by using a lower bound to eliminate suboptimal paths.
#' 
#' The algorithm starts from a root node (an empty path) and recursively explores all possible tours by 
#' adding unvisited cities one by one, while applying the bound function to prune branches of the search tree.
#' Once all cities are visited, the function checks if the cost of the tour is better than the current best tour.
#'
#' @param data A numeric matrix or data frame where each row represents a city and each column represents 
#'             its coordinates (for a 2D TSP).
#' 
#' @return A list containing the following elements:
#' \describe{
#'   \item{best_tour}{A numeric vector representing the order of cities in the optimal tour. The class attribute is set to "TSP".}
#'   \item{best_cost}{The total cost of the optimal tour.}
#'   \item{nb_call}{The total number of recursive calls made during the algorithm execution.}
#' }
#' 
#' @examples
#' # Generate a random set of cities
#' cities <- matrix(runif(10), ncol = 2)
#' # Solve TSP using Branch and Bound
#' result <- TSP_B_and_B(cities)
#' print(result$best_tour)
#' print(result$best_cost)
#'
#' @export
TSP_B_and_B <- function(data)
{
  n <- nrow(data)  # Nombre de villes
  distances <- as.matrix(dist(data)) # Calculer la matrice des distances
  best_tour <- NULL
  best_cost <- Inf
  nb_call <- 0
  root_node <- list(path = 1, cost = 0, bound = 0, nb_call = 0)

  ##
  ####
  ###### Bound function: Lower bound on the minimum cost of the tour
  ########
  ##########
  lower_bound <- function(path, distances)
  {
    n <- nrow(distances)
    remaining_nodes <- setdiff(1:n, path)
    if (length(remaining_nodes) == 0) return(0)  # All nodes are visited
    remaining_nodes <-c(remaining_nodes, path[1])

    ### FOR start at last node in path
    bound <-  min(distances[path[length(path)], remaining_nodes])
    ### FOR remaining_nodes
    for (i in remaining_nodes[-1])
    {
      # Find the minimum edge distance to any other remaining node
      selection_minus_i <- remaining_nodes[remaining_nodes != i]
      min_edge <- min(distances[i, selection_minus_i])
      bound <- bound + min_edge
    }
    return(bound)
  }
  ##########
  ########
  ######
  ####
  ##

  # Recursive Branch and Bound function
  explore_node <- function(node)
  {
    nb_call <<- node$nb_call + 1
    if (length(node$path) == n)
    {
      node$cost <- node$cost + distances[node$path[n], node$path[1]]  # Complete the tour
      if (node$cost < best_cost)
      {
        best_cost <<- node$cost # global assignment
        best_tour <<- node$path # global assignment
      }
      return()
    }

    # Calculate the bound
    node$bound <- node$cost + lower_bound(node$path, distances)

    # Pruning: If the bound is greater than the best cost, prune this branch
    if (node$bound >= best_cost) { return() }

    # Explore all remaining unvisited nodes
    remaining_nodes <- setdiff(1:n, node$path)

    for (i in remaining_nodes)
    {
      new_node <- list(path = c(node$path, i),
                       cost = node$cost + distances[node$path[length(node$path)], i],
                       bound = 0,
                       nb_call = nb_call)
      explore_node(new_node)
    }
  }

  # Start the algorithm from the root node (empty path, cost 0)
  explore_node(root_node)

  attr(best_tour, "class") <- "TSP"

  return(list(best_tour = best_tour, best_cost = best_cost, nb_call = nb_call))
}





