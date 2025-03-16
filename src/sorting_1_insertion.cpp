#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

#include<vector> //to use std::vector<double>

//' Insertion sort algorithm using C++
 //'
 //' @param v an unsorted vector of numeric data
 //' @return a sorted vector
 //' @export
 // [[Rcpp::export]] //mandatory to export the function
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
 