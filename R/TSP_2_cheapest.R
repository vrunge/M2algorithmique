##  GPL-3 License
## Copyright (c) 2025 Vincent Runge

#' Cheapest Insertion Algorithm for Solving the Traveling Salesman Problem (TSP)
#'
#' @description
#' This function solves the Traveling Salesman Problem (TSP) using the cheapest insertion heuristic. 
#' It generates a tour starting from either one city or all cities (iteratively) as possible starting points, 
#' and progressively inserts the remaining cities at the position that results in the least increase in distance.
#'
#' @param data A numeric matrix or data frame of coordinates where each row represents a city and 
#'             each column represents the x and y coordinates (for a 2D TSP).
#' @param type A character string specifying the type of starting city selection. 
#'             If `type = "one"`, the starting city is randomly chosen. 
#'             If `type = "all"`, the algorithm tests all possible starting cities and selects the best one.
#'
#' @return A numeric vector representing the order of cities in the optimal tour, 
#'         with the class attribute set to "TSP".
#'
#' @examples
#' # Generate a random set of 5 cities
#' cities <- matrix(runif(10), ncol = 2)
#' # Solve TSP using cheapest insertion starting from one city
#' best_tour <- TSP_cheapest(cities, type = "one")
#' # Solve TSP using cheapest insertion testing all starting cities
#' best_tour_all <- TSP_cheapest(cities, type = "all")
#'
#' @export
TSP_cheapest <- function(data, type = "one")
{
  n <- dim(data)[1]
  distances <- as.matrix(dist(data))
  tour <- NULL
  if(type == "all"){start_test <- 1:n}else{start_test <- sample(n,1)}
  for(l in start_test) ### on teste les n villes de départ possibles
  {
    tour[[l]] <- l
    to_visit <- setdiff(1:n, l)
    for(i in 1:(n-1))  #tour avec i villes (boucle d'insertion des villes)
    {
      best_insertion_LGR <- Inf
      for(j in 1:(n-i)) #pour chaque ville restante
      {
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
      }
      tour[[l]] <- tour_temp
      to_visit <- to_visit_temp
    }
  }
  #####
  best_LGR <- Inf
  for(m in start_test) #choisit le meilleur des n tours trouvés
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

