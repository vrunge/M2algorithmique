##  GPL-3 License
## Copyright (c) 2025 Vincent Runge

#' Farthest Insertion Algorithm for Solving the Traveling Salesman Problem (TSP)
#'
#' @description
#' This function solves the Traveling Salesman Problem (TSP) using the farthest insertion heuristic. 
#' The algorithm starts from the farthest pair of cities and then iteratively inserts the next city 
#' at the position that increases the tour distance the least. 
#' The function can also test all possible starting cities if `type = "all"`, using the `greedy_TSP_max_all` helper function.
#'
#' @param data A numeric matrix or data frame of coordinates where each row represents a city and 
#'             each column represents the x and y coordinates (for a 2D TSP).
#' @param type A character string specifying the type of starting city selection. 
#'             If `type = "one"`, the starting city is chosen as the farthest city from an initial random selection. 
#'             If `type = "all"`, the algorithm tests all possible starting cities and selects the best one.
#'
#' @return A numeric vector representing the order of cities in the optimal tour, 
#'         with the class attribute set to "TSP".
#'
#' @examples
#' # Generate a random set of 5 cities
#' cities <- matrix(runif(10), ncol = 2)
#' # Solve TSP using farthest insertion starting from one city
#' best_tour <- TSP_farthest(cities, type = "one")
#' # Solve TSP using farthest insertion testing all starting cities
#' best_tour_all <- TSP_farthest(cities, type = "all")
#'
#' @export
TSP_farthest <- function(data, type = "one")
{
  if(type == "all"){return(greedy_TSP_max_all(data))}
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  max_dist <- which.max(distances)
  tour <- c((max_dist  - 1) %% n + 1, (max_dist - 1)%/% n + 1)
  to_visit <- setdiff(1:n, tour)

  for(i in 2:(n-1))  #tour déjà construit avec  i villes (boucle d'insertion des villes)
  {
    distance_max <- -Inf
    for(j in 1:(n-i)) #pour chaque ville restante (de n-2 à 1)
    {
      distance_min <- Inf
      for(k in 1:i) #on cherche la distance min de cette ville aux i positions
      {
        LGR <-  distances[tour[k], to_visit[j]]
        if(LGR < distance_min)
        {
          distance_min <- LGR
          to_visit_temp <- j
        }
      }
      #### si ce min est plus grand qu'un autre min
      if(distance_max < distance_min)
      {
        distance_max <- distance_min
        to_visit_temp_max <- to_visit_temp
      }
    }
    ### best insertion of to_visit_temp_max
    j <- to_visit_temp_max
    best_insertion_LGR <- Inf
    for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
    {
        if(k < i){LGR <-  distances[tour[k], to_visit[j]] + distances[to_visit[j], tour[k+1]] - distances[tour[k], tour[k+1]]}
        if(k == i){LGR <-  distances[tour[k], to_visit[j]] + distances[to_visit[j], tour[1]] - distances[tour[k], tour[1]]}

        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- append(tour, to_visit[j], after = k)
          to_visit_temp <- to_visit[-j]
        }
    }
    tour <- tour_temp
    to_visit <- to_visit_temp

  }
  attr(tour, "class") <- "TSP"
  return(tour)
}

########################################################################
########################################################################
########################################################################


greedy_TSP_max_all <- function(data)
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL
  
  for(l in 1:n) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)
    
    for(i in 1:(n-1))  #tour déjà construit avec  i villes (boucle d'insertion des villes)
    {
      distance_max <- -Inf
      
      for(j in 1:(n-i)) #pour chaque ville restante (de n-2 à 1)
      {
        distance_min <- Inf
        for(k in 1:i) #on cherche la distance min de cette ville aux i positions
        {
          LGR <-  distances[tour[[l]][k], to_visit[j]]
          if(LGR < distance_min)
          {
            distance_min <- LGR
            to_visit_temp <- j
          }
        }
        #### si ce min est plus grand qu'un autre min
        if(distance_max < distance_min)
        {
          distance_max <- distance_min
          to_visit_temp_max <- to_visit_temp
        }
        
      }
      ### best insertion of to_visit_temp_max
      j <- to_visit_temp_max
      best_insertion_LGR <- Inf
      for(k in 1:i) #on teste l'insertion possible de cette ville aux i positions
      {
        if(k < i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][k+1]] - distances[tour[[l]][k], tour[[l]][k+1]]}
        if(k == i){LGR <-  distances[tour[[l]][k], to_visit[j]] + distances[to_visit[j], tour[[l]][1]] - distances[tour[[l]][k], tour[[l]][1]]}
        
        if(LGR < best_insertion_LGR)
        {
          best_insertion_LGR <- LGR
          tour_temp <- append(tour[[l]], to_visit[j], after = k)
          to_visit_temp <- to_visit[-j]
        }
      }
      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }
  
  #####
  best_LGR <- Inf
  for(m in 1:n) #choisit le meilleur des n tours trouvés
  {
    LGR <-  sum(distances[tour[[m]] + n*(c(tour[[m]][-1], tour[[m]][1]) - 1)])
    if(LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour[[m]]
    }
  }
  
  attr(best_tour, "class") <- "TSP"
  
  return(best_tour)
}
