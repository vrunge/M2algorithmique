#include <Rcpp.h>
#include <cmath>  // for sqrt and pow functions
#include <string> // Include for std::string
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix compute_distances(NumericMatrix data)
{
  int n = data.nrow();  // Number of cities
  NumericMatrix distances(n, n);  // Initialize an n x n distance matrix
  double dist;
  
  // Calculate the pairwise distances
  for (int i = 0; i < n; i++)
  {
    for (int j = i + 1; j < n; j++)
    {
      // Calculate Euclidean distance
      dist = sqrt(pow(data(i, 0) - data(j, 0), 2) + pow(data(i, 1) - data(j, 1), 2));
      distances(i, j) = dist;
      distances(j, i) = dist;  // Symmetric matrix
    }
  }
  return distances;
}

