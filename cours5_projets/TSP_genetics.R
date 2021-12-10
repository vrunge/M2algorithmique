

####

eval_tour <- function(villes, tour)
{
  longueur <- sum(apply(apply(villes[c(tour, tour[1]),],2,function(x) diff(x)^2), 1 ,function(x) sqrt(sum(x))))
  plot(villes[,1], villes[,2], xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "")
  for(i in 1:(length(tour)-1)){segments(x0 = villes[tour[i],1],
                                        y0 = villes[tour[i],2], 
                                        x1 = villes[tour[i+1],1], 
                                        y1 = villes[tour[i+1],2], lwd = 1)}
  segments(x0 = villes[tour[length(tour)],1],
           y0 = villes[tour[length(tour)],2], 
           x1 = villes[tour[1],1], 
           y1 = villes[tour[1],2], lwd = 1)
  return(longueur)
}


### TEST 

n <- 50
villes <- matrix(runif(2*n), n, 2)

tour <- sample(n)
eval_tour(villes, tour)
