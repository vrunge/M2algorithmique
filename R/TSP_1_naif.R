##  GPL-3 License
## Copyright (c) 2025 Vincent Runge


#' Algorithme Naïf pour le Problème du Voyageur de Commerce (TSP)
#'
#' Résout le problème du voyageur de commerce (TSP) en utilisant une approche heuristique naïve
#' basée sur l'insertion des villes dans le trajet à chaque étape. Le meilleur trajet est déterminé
#' en testant un ou tous les points de départ possibles (option type).
#'
#' @param data Une matrice ou un \code{data.frame} représentant les coordonnées des villes. Les
#'             lignes doivent correspondre aux villes et les colonnes aux coordonnées (par exemple,
#'             \code{x} et \code{y} dans un espace 2D).
#' @param type Un caractère indiquant la manière de choisir le point de départ :
#'             \itemize{
#'               \item{"one"} (par défaut) : Une ville de départ est choisie aléatoirement.
#'               \item{"all"} : Teste toutes les villes comme point de départ et choisit le meilleur trajet.}
#'
#' @details
#' L'algorithme fonctionne en choisissant un point de départ (soit aléatoirement, soit en testant toutes
#' les villes) et en insérant les villes restantes à chaque étape. La ville qui minimise la distance
#' totale est ajoutée à chaque étape du trajet jusqu'à ce que toutes les villes soient visitées.
#'
#' Une fois tous les trajets testés (en fonction de la stratégie de départ choisie), la fonction retourne
#' le trajet avec la distance totale la plus courte.
#'
#' @return
#' Un vecteur représentant l'ordre des villes à visiter pour obtenir le trajet le plus court, dans
#' le cadre du problème du voyageur de commerce. Ce vecteur a un attribut \code{class} défini à
#' \code{"TSP"}.
#'
#' @examples
#' 
#' n <- 10
#' villes <- matrix(runif(2*n), n, 2)
#' TSP_naif(villes, type = "one")
TSP_naif <- function(data, type = "one")
{
  n <- nrow(data)  # Nombre de villes
  distances <- as.matrix(dist(data)) # Calculer la matrice des distances

  # Initialisation des variables
  best_tour <- NULL
  best_LGR <- Inf

  # Choisir les villes de départ
  if (type == "all"){start_test <- 1:n} else {start_test <- sample(n, 1)}


  # Tester chaque ville de départ (si type == "all", tester toutes les villes)
  for (start in start_test)
  {
    # Tour à partir de la ville de départ
    tour <- integer(n)
    tour[1] <- start

    to_visit <- setdiff(1:n, start)

    # Insérer les villes restantes dans le tour
    for (i in 2:n)
    {
      # Calculer les distances entre la ville actuelle et les villes restantes
      remaining_distances <- distances[tour[i - 1], to_visit]
      best_insertion <- which.min(remaining_distances)
      # Insérer la meilleure ville
      tour[i] <- to_visit[best_insertion]
      to_visit <- to_visit[-best_insertion]  # Retirer la ville insérée des villes restantes
    }

    # Calculer la distance totale pour le tour actuel
    LGR <- sum(distances[cbind(tour, c(tour[-1], tour[1]))])

    # Mettre à jour le meilleur tour trouvé
    if (LGR < best_LGR)
    {
      best_LGR <- LGR
      best_tour <- tour
    }
  }
  attr(best_tour, "class") <- "TSP"
  return(best_tour)
}


