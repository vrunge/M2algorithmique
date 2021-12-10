
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# PROJET 1

graph <- function(n, nbPaths)
{
  x <-  runif(n-2)
  y <-  runif(n-2)
  x <- c(0,x,1)
  y <- c(0,y,1)
  edge <- matrix(0, (n-1)*nbPaths, 3)
  colnames(edge) <- c("v1","v2", "distance")

  for(i in 1:nbPaths)
  {
    pos <- ((i-1)*(n-1) +1):(i*(n-1))
    U <- sample(2:(n-1))
    S <- c(1,U)
    E <- c(U,n)
    edge[pos, 1] <- S
    edge[pos, 2] <- E
    edge[pos, 3] <- sqrt((x[S] - x[E])^2 +  (y[S] - y[E])^2)  
  }

  return(list(x = x, y = y, edge = edge))
}

n <- 100
res <- graph(n,20)
res
plot(res$x, res$y, xlim = c(0,1), ylim = c(0,1))
for(i in 1:dim(res$edge)[1]){segments(x0 = res$x[res$edge[i,1]],
                      y0 = res$y[res$edge[i,1]], 
                      x1 = res$x[res$edge[i,2]], 
                      y1 = res$y[res$edge[i,2]], lwd = 0.1)}
points(res$x[1], res$y[1], col = 2, cex = 2)
points(res$x[n], res$y[n], col = 2, cex = 2)


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# PROJET 2


changeVarianceData <- function(n, tau = c(0,0.5,1), sd = c(0,2))
{
  res <- rep(0,n)
  chpt <- floor(n * tau)
  res <- rnorm(n = n, mean = 0, sd = rep(sd, diff(chpt)))
  return(list(data = res, tau = tau, sd = sd))
}

plot(changeVarianceData(1000, c(0,0.3,0.5,0.8,1), c(1,10,1,5))$data, type = 'l')

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# PROJET 3


binaryMatrix <- function(n, p, nbClust = 10 , d1 = 0.8, d0 = 0.2)
{
  mat <- matrix(rbinom(n = n*p, size = 1, prob = d0),n,p)
  clus1 <- rpois(nbClust, 10)
  divX <- floor(clus1* n / sum(clus1))
  divX[1] <- divX[1] + (n - sum(divX))
  divX <- c(1, cumsum(divX))
  clus2 <- rpois(nbClust, 10)
  divY <- floor(clus2* p / sum(clus2))
  divY[1] <- divY[1] + (p - sum(divY))
  divY <- c(1, cumsum(divY))
  for(i in 1:nbClust)
  {
    mat[divX[i]:divX[i+1],divX[i]:divX[i+1]] <- rbinom(n = (divX[i+1]-divX[i]+1)*(divX[i+1]-divX[i]+1), size = 1, prob = d1)
  }
  
  return(list(matI = mat, matR = mat[sample(n),sample(p)]))
}

res <- binaryMatrix(500,500)
image(res$matI)
image(res$matR)


##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# PROJET 4

studentProjectScore <- function(n, type = "random", pref = (1:n)^2)
{
  res <-  NULL
  if(type == "pref")
  {
    res <- matrix(1:n,n,n, byrow = TRUE)
    res <- t(apply(res,1,function(x) sample(x, prob = pref)))
  }
  if(type == "random")
  {
    res <- matrix(1:n,n,n, byrow = TRUE)
    res <- t(apply(res,1,sample))
  }
  return(res)
}



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Projet 5
# web scraping

# http://edutechwiki.unige.ch/fr/Web_scraping_avec_R




##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Projet 6

dataSeries <- function(n0 = 100, n1 = 100, p0 = 0.3, p1 = 0.8)
{
  s1 <- rbinom(n = n0, size = 1, prob = p0)
  s2 <- rbinom(n = n1, size = 1, prob = p1)
  return(c(s1,s2))
}

plot(dataSeries(), type = 'l')



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
#Projet 7

n <- 1000
#x <- rnorm(n, mean = 2)
x <- runif(n, min = 0, max = 4)

# CASE 1
y <- 3*x + 2
plot(x,y)
cor(x,y)
cor(x,y, method="kendall")

# CASE 2
y <- x*(1-x)
plot(x,y)
cor(x,y)
cor(x,y, method="kendall")

# CASE 3
z <- (x-2)^9
y <- z + abs(z)/max(abs(z))*rnorm(n, mean = 0, sd = 100) 
plot(x,y)
cor(x,y)
cor(x,y, method="kendall")



#### resampling


myCorr <- function(x,y, n, nb)
{
  s <- sample(n, size = nb)
  return(cor(x[s],y[s], method="kendall"))
}

z <- replicate(1000, myCorr(x,y,n,100))
hist(z, breaks = 30)
mean(z)



