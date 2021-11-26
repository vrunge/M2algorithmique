
### ### ### ### ### ### ### ### ### 
### fonctions découpe de barres ### 
### ### ### ### ### ### ### ### ### 

barre <- function(p,n)
{
  print(n) 
  if(n == 0)
  { 
    cat("\n")
    return(0)
  }
  q <- -Inf
  for(i in 1:n)
  {
    q <- max(q, p[i] + barre(p, n-i))
  }

  return(q)
}


barre(c(1,5,8,9,10,17,17,20,24,30),4)

### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### 

barreMEMO <- function(p,n)
{
 r <- rep(-Inf, n)
 return(barreAUX(p,n,r))
}

barreAUX <- function(p,n,r)
{
  print(r)
  print(n) ### afficher n
  if(n == 0)
  { 
    cat("\n")
    return(0)
  }
  if(r[n] >= 0){return(r[n])}
  q <- -Inf
  for(i in 1:n)
  {
    q <- max(q, p[i] + barreAUX(p, n-i,r))
  }
  r[n] <- q
  return(q)
}

barreMEMO(c(1,5,8,9,10,17,17,20,24,30),9) 

#ça ne marche pas...
#ça ne marche pas...
#ça ne marche pas...


### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### 


barreMemoise <- local({
  memory <- list()
  memory[["nbCall"]] <- 0
  appel <- 0
  function(p,n) {
    memory[["nbCall"]] <<- memory[["nbCall"]] + 1 ### compter les appels
    print(n) #affiche le n appelé
    print(unlist(memory)) ### affiche la mémoire
    appel <- appel + 1
    valueName <- as.character(n)
    if (!is.null(memory[[valueName]])) return(memory[[valueName]])
    if(n == 0){return(0)}
    q <- -Inf
    for(i in 1:n){q <- max(q, p[i] + Recall(p, n-i))}
    memory[[valueName]] <<- q # store results
    q
  }
})

barreMemoise(c(1,5,8,9,10,17,17,20,24,30),10)

10*11/2

#source : https://www.r-bloggers.com/2018/10/optimize-your-r-code-using-memoization/

### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### 

barreFORWARD <- function(p,n)
{
  r <- rep(0, n+1)
  s <- rep(0, n+1)
  for(j in 2:(n+1))
  {
    q <- -Inf
    for(i in 1:(j-1))
    {
      if(q < p[i] + r[j-i])
      {
        q <- p[i] + r[j-i]
        s[j] <- i 
      }
    r[j] <- q 
    }
  }
  
  ##### backtracking #####
  vec <- NULL
  length <- n+1
  while(length > 1)
  {
    vec <- c(s[length], vec) 
    length <- length - s[length] 
  }
  
  return(list(coutOptimal = q, decoupeOptimale = vec))
}

barreFORWARD(c(1,5,8,9,10,17,17,20,24,30),10)


### ### ### ### ### ### ### 
### fonctions FIBONACCI ###
### ### ### ### ### ### ###

fibo <- function(n)
{
  if(n == 0){return(0)}
  if(n == 1){return(1)}
  fibo(n-1) + fibo(n-2)
}


fibo2 <- function(n)
{
  prev <- 1
  curr <- 0
  for(i in 1:n)
  {
    nex <-  curr + prev
    prev <- curr
    curr <- nex
  }
  return(curr)
}


fibo(35)
fibo2(35)


### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### 




