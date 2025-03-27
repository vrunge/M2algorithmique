#include <Rcpp.h>
#include <vector>
#include <algorithm> // For std::swap

using namespace Rcpp;

//' Maintains the heap property by sifting down an element
//'
//' @description
//' This function ensures the \bold{max-heap property} in a given numeric vector. 
//' It is used internally by heap sort to reorganize the vector into a valid max-heap.
//'
//' @param heap A numeric vector representing a heap.
//' @param i The index of the current element to heapify (1-based indexing).
//' @param n The number of elements in the heap.
//' @return The modified heap with the max-heap property restored.
//' @note This function modifies the input vector in-place.
//'
//' @keywords internal
void heapify(std::vector<double>& heap, size_t i, size_t n)
{
  size_t largest = i;
  size_t left = 2 * i;
  size_t right = 2 * i + 1;
   
  // Convert to 0-based indexing (C++)
  size_t index = i - 1;
  size_t leftIndex = left - 1;
  size_t rightIndex = right - 1;
   
  if (left <= n && heap[leftIndex] > heap[index])
  {
    largest = left;
  }
  if (right <= n && heap[rightIndex] > heap[largest - 1])
  {
    largest = right;
  }
  if (largest != i)
  {
    std::swap(heap[index], heap[largest - 1]);
    heapify(heap, largest, n);
  }
}
 
 
//' Heap Sort Algorithm (C++ Implementation)
//'
//' @description
//' This function implements \bold{Heap Sort}, an efficient sorting algorithm that 
//' first builds a max-heap and then repeatedly extracts the maximum element to 
//' produce a sorted sequence.
//'
//' The algorithm consists of two main steps:
//' \enumerate{
//'   \item \bold{Heap Construction:} The input vector is reorganized into a max-heap.
//'   \item \bold{Sorting:} The largest element (root) is swapped with the last element, 
//'     reducing the heap size and reapplying heapify.
//' }
//'
//' @param v A numeric vector containing unsorted elements.
//' @return A numeric vector sorted in ascending order.
//'
//' @details
//' \itemize{
//'   \item \bold{Time Complexity:}
//'     \itemize{
//'       \item Building the heap: O(n)
//'       \item Extracting elements: O(n log n)
//'       \item Overall: O(n log n)
//'     }
//'   \item \bold{Space Complexity:} O(1) (in-place sorting).
//'   \item \bold{Unstable Sort:} The relative order of equal elements may change.
//' }
//' @examples
//' heap_sort_Rcpp(rnorm(100))
//' heap_sort_Rcpp(sample(100))
//'
//' @export
// [[Rcpp::export]]
std::vector<double> heap_sort_Rcpp(std::vector<double> v)
{
  size_t n = v.size();
  // Build max heap
  for (size_t i = n / 2; i > 0; i--)
  {
    heapify(v, i, n);  // Passed by reference (modifies vector)
  }
  // Extract elements one by one
  for (size_t i = n; i > 1; i--)
  {
    std::swap(v[0], v[i - 1]);  // Move current root to the end
    heapify(v, 1, i - 1);        // Heapify reduced heap (pass by reference)
  }
  return v;
}


