
library(optR)

###
### one simu for linear regression solution
###

one.simu <- function(i,n,p, type = "inverse")
{
  beta <- sample(p)
  X <- matrix(rnorm(n*p),n,p)
  Y <- X%*%beta + rnorm(n)
  
  if(type == "inverse")
  {
    start.time <- Sys.time()
    solve(t(X)%*%X)%*%t(X)%*%Y
    end.time <- Sys.time() 
  }
  if(type == "lm")
  {
    start.time <- Sys.time()
    lm(Y~X-1)
    end.time <- Sys.time() 
  }
  if(type == "gauss")
  {
    start.time <- Sys.time()
    p <- nrow(X)
    U.pls <- cbind(X,Y)
    U.pls[1,] <- U.pls[1,] / U.pls[1,1]
    i <- 2
    while(i < p + 1) 
    {
      j <- i
      while(j < p + 1) 
      {
        U.pls[j, ] <- U.pls[j, ] - U.pls[i-1, ] * U.pls[j, i-1]
        j <- j+1
      }
      while(U.pls[i,i] == 0){U.pls <- rbind(U.pls[-i,], U.pls[i,])}
      U.pls[i,] <- U.pls[i,]/U.pls[i,i]
      i <- i+1
    }
    for(i in p:2)
    {
      for(j in i:2-1){U.pls[j, ] <- U.pls[j, ] - U.pls[i, ] * U.pls[j, i]}
    }
    U.pls[, p + 1]
    end.time <- Sys.time() 
  }
  return((end.time - start.time)[[1]])
}


###
### HOW TO DO well-done simulations !
###


##################################
###### Initialize dataframe ######
##################################



nb_Simus <- 20
nb_n <- 20
all_n <- round(seq(from = 200, to = 800, length.out = nb_n))
p <- 10



df <- data.frame(matrix( nrow = 3*length(all_n), ncol = 3 + nb_Simus))
colnames(df) <- c("type", "n", "p", 1:nb_Simus)
dim(df)


####################################
###### simulations multicores ######
####################################

library(parallel) 
nbCores <- 1 ## set to 1 to get the rigtht computation time
j <- 1

for(myN in all_n)
{
  print(c("n =", myN))
  liste1 <- mclapply(1:nb_Simus, FUN = one.simu,
                     n = myN,
                     p = myN,
                     type = "inverse",
                     mc.cores = nbCores)
  liste2 <- mclapply(1:nb_Simus, FUN = one.simu,
                     n = myN,
                     p = myN,
                     type = "lm",
                     mc.cores = nbCores)
  liste3 <- mclapply(1:nb_Simus, FUN = one.simu,
                     n = myN,
                     p = myN,
                     type = "gauss",
                     mc.cores = nbCores)
  df[j ,] <- c("inverse", myN, p, do.call(cbind, liste1))
  df[j+1, ] <- c("lm", myN, p, do.call(cbind, liste2))
  df[j+2, ] <- c("gauss", myN, p, do.call(cbind, liste3))
  j <- j + 3
}


##########################################
###### saving dataframe in csv file ######
##########################################

write.csv(df, "regression_time.csv", row.names = FALSE)




library(ggplot2)
library(cowplot)
library(reshape2)

############################################
###### summary function for quantiles ######
############################################

data_summary <- function(data, varname, groupnames)
{
  require(plyr)
  summary_func <- function(x, col)
  {
    c(mean = mean(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], 0.025), q3 = quantile(x[[col]], 0.975))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



############################################
###### read and transform dataframe ########
############################################

MyData <- read.csv(file="regression_time.csv", header = TRUE)
df <- melt(MyData, id.vars = c("type","n","p"))

dfnew <- df[,-4]

dfnew2 <- data_summary(dfnew, varname="value",
                       groupnames=c("type","n"))

theMin <- min(dfnew2[,3:5], dfnew2[,3:5])
theMax <- max(dfnew2[,3:5], dfnew2[,3:5])

################################
###### PLOT with ggplot2 #######
################################

# Everything on the same plot
ggplot(dfnew2, aes(x = n, y = value, col=type)) +  scale_x_log10()+ scale_y_log10(limits = c(theMin, theMax))  +
  labs(y = "time in seconds") +  labs(x = "data length") +
  geom_point(size = 2, aes(shape = type)) +
  geom_errorbar(aes(ymin=`q1.2.5%`, ymax=`q3.97.5%`), width=.01) +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.text=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank())


################# coefficients and analysis #################
################# coefficients and analysis #################
################# coefficients and analysis #################

res_inv <- dfnew2[dfnew2$type == "inverse",c(2,3)]
res_lm <- dfnew2[dfnew2$type == "lm",c(2,3)]
res_gauss <- dfnew2[dfnew2$type == "gauss",c(2,3)]

LMres_inv <- lm(log(value) ~ log(n), data = res_inv, )
LMres_lm <- lm(log(value) ~ log(n), data = res_lm, )
LMres_gauss <- lm(log(value) ~ log(n), data = res_gauss, )

summary(LMres_inv)
summary(LMres_lm)
summary(LMres_gauss)

LMres_inv$coefficients
LMres_lm$coefficients
LMres_gauss$coefficients

