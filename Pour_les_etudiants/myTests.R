

################################################
############# A simple test ####################
################################################

n <- 100 #data size 
s <- sample(n) #generate data


# the 4 algorithms

insertion_sort(s)
heap_sort(s)
insertion_sort_Rcpp(s)
heap_sort_Rcpp(s)


################################################################################################
# We define the function one.simu which returns the execution time of a given algorithm
one.simu <- function(n, type = "sample", func = "insertion_sort")
{
  if(type == "sample"){v <- sample(n)}else{v <- n:1}
  if(func == "insertion_sort"){t <- system.time(insertion_sort(v))[[1]]}
  if(func == "heap_sort"){t <- system.time(heap_sort(v))[[1]]} 
  if(func == "insertion_sort_Rcpp"){t <- system.time(insertion_sort_Rcpp(v))[[1]]}
  if(func == "heap_sort_Rcpp"){t <- system.time(heap_sort_Rcpp(v))[[1]]}
  return(t)
}
################################################################################################

###########################################################
############# One time complexity test ####################
###########################################################
#we evaluate the time with a given n for the 4 algorithms
n <- 10000
one.simu(n, func = "insertion_sort")
one.simu(n, func = "heap_sort")
one.simu(n, func = "insertion_sort_Rcpp")
one.simu(n, func = "heap_sort_Rcpp")

######################################################################### 
############# A short simulation study at fixed vector size ############# 
######################################################################### 

#we compare the running time at a given length n with repeated executions (nbSimus times)
nbSimus <- 10
time1 <- 0
time2 <- 0
time3 <- 0
time4 <- 0
for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "insertion_sort")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "heap_sort")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "insertion_sort_Rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "heap_sort_Rcpp")}

#gain R -> Rcpp
time1/time3
time2/time4

#gain insertion -> heap
time1/time2
time3/time4

#max gain
time1/time4


####### MY RESULT ####### 
#> #gain R -> Rcpp
#  > time1/time3
#[1] 154.6497
#> time2/time4
#[1] 184.1053
#> 
#  > #gain insertion -> heap
#  > time1/time2
#[1] 8.709548
#> time3/time4
#[1] 10.36842
#> 
#  > #max gain
#  > time1/time4
#[1] 1603.474


#HERE : R to Rcpp => at least 150 times faster
#HERE : insertion to heap => 10 times faster


##########################################
############# microbenchmark ############# 
##########################################


library(microbenchmark)
library("ggplot2")
n <- 10000
res <- microbenchmark(one.simu(n, func = "insertion_sort_Rcpp"), one.simu(n, func = "heap_sort_Rcpp"), times = 50)
autoplot(res)
res


##########################################
############# time complexity ############ 
##########################################


nbSimus <- 10
vector_n <- seq(from = 10000, to = 100000, length.out = nbSimus)
nbRep <- 10
res_Heap <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_Heap) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_Heap[j,] <- c(i, replicate(nbRep, one.simu(i, func = "heap_sort_Rcpp")))  
  print(j)
  j <- j + 1
}

res <- rowMeans(res_Heap[,-1])
plot(vector_n, res, xlab = "data length", ylab = "time in second")


####

nbSimus <- 40
vector_n <- seq(from = 5000, to = 50000, length.out = nbSimus)
nbRep <- 10
res_Insertion <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_Insertion) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_Insertion[j,] <- c(i, replicate(nbRep, one.simu(i, func = "insertion_sort_Rcpp")))  
  print(j)
  j <- j + 1
}

res <- rowMeans(res_Insertion[,-1])
plot(vector_n, res, xlab = "data length", ylab = "time in second")
lm(log(res) ~ log(vector_n))




