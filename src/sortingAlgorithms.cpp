#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

#include<vector> //to use std::vector<double>

//' Insertion sort algorithm using C++
//'
//' @param v an unsorted vector of numeric data
//' @return a sorted vector
// [[Rcpp::export]] //mandatory to export the function
std::vector<double> insertion_sort_Rcpp(std::vector<double> v)
{
  double key;
  unsigned int i;
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



// [[Rcpp::export]]
NumericVector build_heap_Rcpp(NumericVector heap, unsigned int i, unsigned int n)
{
  unsigned int k = i;
  unsigned int l = 2*k;
  double temp;
  while(l <= n)
  {
    if((l < n) && (heap[l-1] < heap[l])){l = l + 1;}
    if(heap[k-1] < heap[l-1])
    {
      temp = heap[k-1];
      heap[k-1] = heap[l-1];
      heap[l-1] = temp;
      k = l;
      l = 2*k;
    }
    else
    {
      l = n + 1;
    }
  }
  return(heap);
}


//' Heap sort algorithm using C++
//'
//' @param v an unsorted vector of numeric data
//' @return a sorted vector
// [[Rcpp::export]]
NumericVector heap_sort_Rcpp(NumericVector v)
{
  unsigned int n = v.size();
  double temp;
    for(unsigned int i = (n/2); i > 0; i--)
    {
      build_heap_Rcpp(v, i, n);
    }
    for(unsigned int i = n; i > 1; i--)
    {
      temp = v[i-1];
      v[i-1] = v[0];
      v[0] = temp;
      build_heap_Rcpp(v, 1, i-1);
    }

  return v;
}
