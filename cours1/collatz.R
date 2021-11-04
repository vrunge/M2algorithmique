collatz <-function(n)
{
  print(n)
  if(n == 1){return("stop")}
  if(n%%2 == 0){collatz(n/2)}else{collatz(3*n+1)}
}

collatz(1000)

###

Collatz <- function(start = 10)
{
  vec <- start
  temp <- start
  while(temp != 1)
  {
    if(temp%%2 == 0)
    {
      vec <- c(vec, temp/2)
      temp <- temp/2
    }
    else
    {
      vec <- c(vec, 3*temp + 1)
      temp <- 3*temp + 1
    }
  }
  return(vec)
}

#####


plotCollatz <- function(vect)
{
  plot(vect, type = "b", lty = 2, pch = 19)
}



CollatzLength <- function(vec = 10:100)
{
  res <- NULL
  for(i in vec)
  {
    res <- c(res, length(Collatz(i)))
  }
  return(res)
}



###
### TESTS 
###

v <- Collatz(500)
plotCollatz(v)

v <- Collatz(1113)
plotCollatz(v)


w <- CollatzLength((1:3000))
plot(w, type = "p", lty = 1, pch = 20, cex = 0.2)

