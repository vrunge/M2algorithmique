

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


studentProjectScore(10, type = "random")


studentProjectScore(10, type = "pref")
