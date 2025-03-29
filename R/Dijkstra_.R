##  GPL-3 License
## Copyright (c) 2025 Vincent Runge






dijkstra <- function(graph, start, end) 
{
  # Initialisation
  n <- nrow(graph)
  distances <- rep(Inf, n)
  distances[start] <- 0
  visited <- rep(FALSE, n)
  previous <- rep(NA, n)
  
  for (i in 1:n) {
    # Sélection du sommet non visité avec la plus petite distance
    min_dist <- Inf
    min_index <- -1
    for (j in 1:n) {
      if (!visited[j] && distances[j] < min_dist) {
        min_dist <- distances[j]
        min_index <- j
      }
    }
    
    if (min_index == -1) break # Tous les sommets accessibles ont été visités
    
    visited[min_index] <- TRUE
    
    # Mise à jour des distances des voisins
    for (j in 1:n) {
      if (graph[min_index, j] > 0 && !visited[j]) { # Il existe un arc
        new_dist <- distances[min_index] + graph[min_index, j]
        if (new_dist < distances[j]) {
          distances[j] <- new_dist
          previous[j] <- min_index
        }
      }
    }
  }
  
  return(list(distances = distances, previous = previous))
}