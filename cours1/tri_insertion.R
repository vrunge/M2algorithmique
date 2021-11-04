
####
#### insertion sorting function
####

insertionsort_function <- function(v)
{
  for (j in 2:length(v)) 
  {
    key <- v[j] 
    i <- j - 1 
    while (i > 0 && v[i] > key)
    {
      v[i + 1] <-  v[i]
      i <- i - 1 
    }
    v[i + 1] <- key
  }
  return(v)
} 


####
#### simu characteristics
####

nb_Simus <- 2
nb_n <- 20
all_n <- seq(from = 10^3, to = 10^4, length.out = nb_n)


####
#### simu function
####

simu_tri <- function(all_n, nb_Simus, type = "rev")
{
  nb_n <- length(all_n)
  res <- rep(0,nb_n)
  
  for(j in 1:nb_n)
  {
    print(j)
    n <- all_n[j]
    temp <- 0
    for(i in 1:nb_Simus)
    {
      if(type == "rev")
      {
        start.time  <- Sys.time()
        insertionsort_function(n:1)
        end.time <- Sys.time()  
      }
      if(type == "sample")
      {
        start.time  <- Sys.time()
        insertionsort_function(sample(n))
        end.time <- Sys.time()  
      }
      temp <-  temp + (end.time - start.time)[[1]]
    }
    res[j] <- temp/nb_Simus
  }
  return(res)
}

####
#### simu function
####

res <- simu_tri(all_n, nb_Simus, "rev")

plot(all_n, res, xlab = "taille des données", ylab = "temps moyen en secondes", type = 'b')
plot(log(all_n), log(res), xlab = "log taille des données", ylab = "log temps moyen en secondes", type = 'b')
lm(log(res) ~ log(all_n))


res2 <- simu_tri(all_n, nb_Simus, "sample")

plot(all_n, res2, xlab = "taille des données", ylab = "temps moyen en secondes", type = 'b')
plot(log(all_n), log(res2), xlab = "log taille des données", ylab = "log temps moyen en secondes", type = 'b')
lm(log(res2) ~ log(all_n))

#####
##### one test : comparing 2 algorithms
#####

devtools::install_github("vrunge/M2algorithmique")
library(M2algorithmique)

n <- 10^6
v <- n:1

system.time(insertion_sort_Rcpp(v))[[1]]
system.time(heap_sort_Rcpp(v))[[1]]




