#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

#include<vector> //to use std::vector<double>

//' Insertion Sort Algorithm (C++ Implementation)
//'
//' @description
//' This function implements the \bold{Insertion Sort} algorithm in C++ and is exported to R using Rcpp. 
//' Insertion Sort is a simple sorting algorithm that builds the sorted sequence one element at a time 
//' by inserting each new element into its correct position within the sorted part of the vector.
//'
//' The algorithm works by iterating through the input vector and shifting larger elements to the right 
//' before inserting the current element (key) into its proper position.
//'
//' @param v A numeric vector containing unsorted elements.
//'
//' @return A numeric vector of the same length as `v`, sorted in ascending order.
//'
//' @details
//'  \itemize{
//'   \item \bold{Time Complexity:} O(n²) in the worst and average cases, O(n) in the best case (already sorted data).
//'   \item \bold{Space Complexity:} O(1) (in-place sorting, no additional memory used).
//'   \item \bold{Stable Sorting Algorithm:} Maintains the relative order of equal elements.
//' }
//' @examples
//' insertion_sort_Rcpp(rnorm(100))
//' insertion_sort_Rcpp(sample(100))
//'
//' @export
// [[Rcpp::export]] // Mandatory to export the function to R
std::vector<double> insertion_sort_Rcpp(std::vector<double> v)
{
  double key;
  int i;
  for(unsigned int j = 1; j < v.size(); j++)
  {
    key = v[j];
    i = j - 1;
    while(i >= 0 && v[i] > key)
    {
      v[i + 1] = v[i];
      i = i - 1;
    }
    v[i + 1] = key;
  }
  return v;
}

