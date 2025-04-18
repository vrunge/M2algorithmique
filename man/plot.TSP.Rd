% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TSP_plot.R
\name{plot.TSP}
\alias{plot.TSP}
\title{Plot a TSP Tour}
\usage{
\method{plot}{TSP}(tour, data, main = "", value = "")
}
\arguments{
\item{tour}{An integer vector of class `TSP` representing the order of cities in the TSP solution. Each element of `tour` corresponds to the index of a city in `data`.}

\item{data}{A numeric matrix or data frame containing the coordinates of the cities. The first column should represent the x-coordinates, and the second column should represent the y-coordinates of the cities.}

\item{main}{A character string that will be displayed as a title for the plot. This is optional and defaults to an empty string.}

\item{value}{A character string that will be displayed as a subtitle for the plot. This is optional and defaults to an empty string.}
}
\description{
This function plots the cities and the corresponding tour path on a 2D plane, 
showing the path connecting each city in the order given by the `tour` vector.
}
\details{
The function uses the `tour` vector to draw a path between cities in the order specified,
and colors each segment based on its position in the sequence. The cities' coordinates are
taken from the `data` input, where each row corresponds to a city.

The plot shows the cities as points and the path as colored segments between them.
The colors of the segments range from blue to red, where the first segment is blue 
and the last segment is red.
}
\examples{
# Example usage
tour <- c(1, 2, 4, 3)
data <- data.frame(x = c(0, 1, 3, 5), y = c(0, 2, 3, 1))
plot.TSP(tour, data, main = "TSP Solution")

}
