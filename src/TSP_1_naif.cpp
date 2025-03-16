#include <Rcpp.h>
using namespace Rcpp;

//' tour_superSimple
 //'
 //' @param n an integer
 //' @return nothing interesting... cities from 1 to n
 //' @export
 // [[Rcpp::export]]
 NumericVector tour_superSimple(int n)
 {
   NumericVector v (n);
   int *list = new int [n];
   for (int i = 0; i < n; i++)
   {
     list[i] = i+1;
   }
   for (int i = 0; i < n; i++)
   {
   }
   return v;
 }
