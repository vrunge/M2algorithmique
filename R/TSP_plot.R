
#' Plot a TSP Tour
#'
#' This function plots the cities and the corresponding tour path on a 2D plane, 
#' showing the path connecting each city in the order given by the `tour` vector.
#'
#' @param tour An integer vector of class `TSP` representing the order of cities in the TSP solution. Each element of `tour` corresponds to the index of a city in `data`.
#' @param data A numeric matrix or data frame containing the coordinates of the cities. The first column should represent the x-coordinates, and the second column should represent the y-coordinates of the cities.
#' @param main A character string that will be displayed as a title for the plot. This is optional and defaults to an empty string.
#' @param value A character string that will be displayed as a subtitle for the plot. This is optional and defaults to an empty string.
#'
#' @details
#' The function uses the `tour` vector to draw a path between cities in the order specified,
#' and colors each segment based on its position in the sequence. The cities' coordinates are
#' taken from the `data` input, where each row corresponds to a city.
#'
#' The plot shows the cities as points and the path as colored segments between them.
#' The colors of the segments range from blue to red, where the first segment is blue 
#' and the last segment is red.
#'
#' @examples
#' # Example usage
#' tour <- c(1, 2, 4, 3)
#' data <- data.frame(x = c(0, 1, 3, 5), y = c(0, 2, 3, 1))
#' plot.TSP(tour, data, main = "TSP Solution")
#'
plot.TSP <- function(tour, data, main = "", value = "")
{
  plot(data[,1], data[,2], 
       xlim = c(min(data[,1]), max(data[,1])),
       ylim = c(min(data[,2]), max(data[,2])), 
       xlab = "", ylab = "", main = main, asp = 1, xaxt = "n", yaxt = "n")
  n <- length(tour)
  col_fun <- colorRamp(c("blue", "red"))
  rgb_cols <- col_fun(1:n / n)
  cols <- rgb(rgb_cols, maxColorValue = 256)
  for(i in 1:(n-1)){segments(x0 = data[tour[i],1],
                             y0 = data[tour[i],2],
                             x1 = data[tour[i+1],1],
                             y1 = data[tour[i+1],2], lwd = 1, col = cols[i])}
  segments(x0 = data[tour[length(tour)],1],
           y0 = data[tour[length(tour)],2],
           x1 = data[tour[1],1],
           y1 = data[tour[1],2], lwd = 1, col = cols[n])
  title(sub = value, adj = 1, line = 0.5, font = 4)
}








