

### fonction qui réalise le croisement entre 2 tours
### data = matrice 2 lignes, n colonnes
###

croisement <- function(data)
{
  n <- dim(data)[2]
  p <- sample(n, 1) #position echange
  temp <-  data[2,p] 
  data[2,p] <- data[1,p] 
  data[1,p] <- temp

  while(length(unique(data[1,])) < n)
  {
    p <- setdiff(which(data[1,] == temp), p)
    temp <-  data[2,p] 
    data[2,p] <- data[1,p] 
    data[1,p] <- temp
  }
  return(data)
}


#####

genetics_TSP <- function(data, nb_Chrom = 10, select =  floor(dim(data)[1]/2), mutation = 0.01, iterMax = 100)
{
  n <- dim(data)[1]
  ### matrice des distances entre villes
  distances <- as.matrix(dist(data))
  
  #### initialisation
  chrom <- t(apply(matrix(0, nb_Chrom, n), 1, function(x) sample(n)))
 
  for(i in 1:iterMax)
  {
    print(chrom)
    ### 1) DISTANCES TOURS : distance du tour pour chaque chromosome
    dist_tours <- rep(0, nb_Chrom)
    for(k in 1:nb_Chrom)
    {
      for(j in 1:(n-1)){dist_tours[k] <- dist_tours[k] + distances[chrom[k,j],chrom[k,j+1]]} 
      dist_tours[k] <- dist_tours[k] + distances[chrom[k,n],chrom[k,1]]
    }
    
    ### 2) selection des meilleurs (en nombre select)
    chrom[1:select, ] <- chrom[order(dist_tours)[1:select], ]
    
    ### 3) croisement
    for(k in seq(select + 1, n-1, by = 2))
    {
      chrom[c(k,k+1),] <- croisement(chrom[sample(1:select, size = 2, prob = select:1),])
    }
    if(k < n-1){chrom[n,] <- croisement(chrom[sample(1:select, size = 2, prob = select:1),])[1,]}
    
    ### 3) mutation
    
    
    
  }
  
  return(chrom)
}



####
n <- 10
data <- matrix(runif(2*n), n, 2)
res <- genetics_TSP(data, iterMax = 5)
res

##### PLOT

tour <- res[1,]


plot(data[,1], data[,2], xlim = c(0,1), ylim = c(0,1))
for(i in 1:(length(tour)-1)){segments(x0 = data[tour[i],1],
                                      y0 = data[tour[i],2], 
                                      x1 = data[tour[i+1],1], 
                                      y1 = data[tour[i+1],2], lwd = 1)}
segments(x0 = data[tour[length(tour)],1],
         y0 = data[tour[length(tour)],2], 
         x1 = data[tour[1],1], 
         y1 = data[tour[1],2], lwd = 1)



###





