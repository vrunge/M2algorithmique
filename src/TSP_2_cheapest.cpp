#include <Rcpp.h>
#include <string> 
#include <random>
#include "TSP_auxiliary.h"

using namespace Rcpp;

//' TSP Cheapest Insertion Algorithm (C++ Implementation)
//'
//' @description
//' This function implements the **Cheapest Insertion** heuristic approach for solving the \bold{Traveling Salesman Problem (TSP)} in C++ and is exported to R using Rcpp. 
//' The algorithm builds the tour by inserting cities into the existing tour in the position that minimizes the increase in tour length.
//'
//' The algorithm starts from a specified city and iteratively inserts cities one by one, choosing the insertion position that results in the cheapest (minimum) increase in the total distance at each step. 
//' In "all" mode, the best tour is selected by testing all possible starting cities and computing the total distance for each tour.
//'
//' @param data A numeric matrix or data frame where each row represents a city and each column represents the coordinates of that city (e.g., x and y coordinates in 2D space).
//' @param type A character string specifying how the starting city is chosen. Options include:
//'  \itemize{
//'   \item \code{"one"} (default): Randomly selects a single starting city.
//'   \item \code{"all"}: Tests all cities as possible starting points and selects the best tour.
//' }
//'
//' @return A numeric vector representing the order of cities in the shortest found tour. The vector is assigned the class "TSP".
//'
//' @details
//' The algorithm is a heuristic approach and does not guarantee the global optimal solution, but aims to find a good solution in a reasonable amount of time.
//'  \itemize{
//'   \item \bold{Time Complexity:} O(n³), where `n` is the number of cities. The algorithm computes the best insertion for each city and tries every possible insertion position.
//'   \item \bold{Space Complexity:} O(n²) due to the storage of the distance matrix.
//' }
//' The result is a tour with the shortest found distance, based on the starting point(s) and insertion method.
//'
//' @examples
//' n <- 40
//' villes <- matrix(runif(2*n), n, 2)
//' TSP_cheapest_Rcpp(villes, type = "one")
//' @export
// [[Rcpp::export]] 
IntegerVector TSP_cheapest_Rcpp(NumericMatrix data, std::string type = "one")
{
  int n = data.nrow();  // Number of cities
  
  ////////////////////////////////////////////////////////////////////
  IntegerVector start_test;
  if (type == "all")
  {
    start_test = seq(0, n - 1); // Test all cities
  }
  else
  {
    // Choose starting cities RANDOMLY
    std::random_device rd;                      // Obtain a random number from hardware
    std::mt19937 gen(rd());                     // Seed the generator
    std::uniform_int_distribution<> dis(0, n - 1); // Define the range [0, n-1]
    start_test =  IntegerVector::create(dis(gen)); // Test all cities
  }
  ////////////////////////////////////////////////////////////////////
  
  // Initialize distances, best_tour, best_LGR
  NumericMatrix distances = compute_distances(data); // Calculate distance matrix
  IntegerVector best_tour;
  double best_LGR = std::numeric_limits<double>::infinity();
  
  //////////
  ////////// Test each starting city
  //////////
  for (int start : start_test)
  {
    // Tour starting from the start city
    IntegerVector tour;
    tour.push_back(start);  // Start the tour with the selected city
    
    std::set<int> to_visit;
    for (int i = 0; i < n; i++)
    {
      if (i != start) { to_visit.insert(i); } // Insert all cities except the start city
    }
    
    // Cheapest Insertion: Insert cities one by one
    while (!to_visit.empty()) // While there are still cities to insert
    {
      double best_increase = std::numeric_limits<double>::infinity();
      int best_city = -1;
      int best_pos = -1;
      
      // Try inserting each city at each possible position in the tour
      for (auto city : to_visit)
      {
        for (int i = 0; i < tour.size(); i++) 
        {
          // Calculate the cost of inserting the city between tour[i] and tour[(i+1) % tour.size()]
          double current_increase = distances(tour[i], city) + distances(city, tour[(i + 1) % tour.size()]) - distances(tour[i], tour[(i + 1) % tour.size()]);
          
          if (current_increase < best_increase) 
          {
            best_increase = current_increase;
            best_city = city;
            best_pos = i + 1;
          }
        }
      }
      
      // Insert the best city at the best position
      tour.insert(tour.begin() + best_pos, best_city);
      to_visit.erase(best_city); // Remove the city from the set of cities to visit
    }
  
    // Calculate the total distance for the current tour
    double LGR = 0;
    for (int i = 0; i < n - 1; i++)
    {
      LGR += distances(tour[i], tour[i + 1]);  // Sum distance between consecutive cities
    }
    LGR += distances(tour[n - 1], tour[0]);  // Add the return distance to the start city
    
    // Update the best tour found
    if (LGR < best_LGR) 
    {
      best_LGR = LGR;
      best_tour = tour;
    }
  } 
  //////////
  ////////// Test each starting city END
  //////////
  
  // Add 1 to all elements of the best_tour
  for (int i = 0; i < best_tour.size(); i++) { best_tour[i] += 1; }
  
  // Set the class for the result
  best_tour.attr("class") = "TSP";
  
  return best_tour;
}

