#' Calculate the Total Length of a TSP Tour
#'
#' This function computes the total length of a given TSP tour by calculating the
#' Euclidean distance between each consecutive pair of cities in the `tour` vector.
#'
#' @param tour An integer vector representing the order of cities in the TSP solution.
#'             Each element of `tour` corresponds to the index of a city in `villes`.
#' @param villes A numeric matrix or data frame containing the coordinates of the cities.
#'               The first column should represent the x-coordinates, and the second column
#'               should represent the y-coordinates of the cities.
#'
#' @return
#' A numeric value representing the total length of the TSP tour, which is the sum of
#' the Euclidean distances between consecutive cities in the tour.
#'
#' @details
#' The function first calculates the pairwise Euclidean distance between each city in the tour
#' and sums up all the distances to return the total length of the tour.
#'
#' The distance between two cities \((x_1, y_1)\) and \((x_2, y_2)\) is computed as:
#' \[ D = sqrt ((x_2 - x_1)^2 + (y_2 - y_1)^2) \]
#'
#' @examples
#' # Example usage
#' tour <- c(1, 3, 2, 4)
#' villes <- data.frame(x = c(0, 1, 3, 5), y = c(0, 2, 3, 1))
#' total_length <- tour_length(tour, villes)
#' print(total_length)
#'
#' @export
tour_length <- function(tour, villes)
{
  return(sum(apply(apply(villes[c(tour, tour[1]),],2,function(x) diff(x)^2), 1 ,function(x) sqrt(sum(x)))))
}
