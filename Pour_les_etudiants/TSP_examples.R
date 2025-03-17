n <- 50
villes <- matrix(rchisq(2*n,1), n, 2)
par(mfrow=c(2,2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(1.5,1.5,1,0) + 0.1)
res_naif <- TSP_naif(villes, type = "one")
plot.TSP(tour = res_naif, data = villes)
res_cheap <- TSP_cheapest(villes, type = "one")
plot.TSP(tour = res_cheap, data = villes)
res_near <- TSP_nearest(villes, type = "one")
plot.TSP(tour = res_near, data = villes)
res_far <- TSP_farthest(villes, type = "one")
plot.TSP(tour = res_far, data = villes)

tour_length(res_naif, villes) 
tour_length(res_cheap, villes) 
tour_length(res_near, villes) 
tour_length(res_far, villes) 


library(ggplot2) #ggplot
library(reshape2) #melt
library(parallel) #mclapply

####################################


one.simu_time_TSP <- function(i, data, algo = "naif", type = "one")
{
  if(algo == "naif")
  {
    start_time <- Sys.time()
    TSP_naif(data, type = type)
    end_time  <- Sys.time()
  }
  if(algo == "cheapest")
  {
    start_time <- Sys.time()
    TSP_nearest(data, type = type)
    end_time  <- Sys.time()
  }
  if(algo == "nearest")
  {
    start_time <- Sys.time()
    TSP_nearest(data, type = type)
    end_time  <- Sys.time()
  }
  if(algo == "farthest")
  {
    start_time <- Sys.time()
    TSP_farthest(data, type = type)
    end_time  <- Sys.time()
  }
  return(unclass(end_time - start_time)[1])
}



my_n_vector_LOG <- seq(from = log(10), to = log(100), by = log(10)/40)
my_n_vector <- round(exp(my_n_vector_LOG))
my_n_vector
diff(log(my_n_vector))


p <- 10 ### répétition
df <- data.frame(matrix( nrow = 4 * length(my_n_vector), ncol = 2 + p))
colnames(df) <- c("algo", "n", 1:p)
dim(df)

library(parallel)
detectCores()  


nbCores <- 8
j <- 1

for(n in my_n_vector)
{
  print(n)
  liste1 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = matrix(runif(2*n), n, 2),
                     algo = "naif",
                     mc.cores = nbCores)
  
  liste2 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = matrix(runif(2*n), n, 2),
                     algo = "cheapest",
                     mc.cores = nbCores)
  
  liste3 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = matrix(runif(2*n), n, 2),
                     algo = "nearest",
                     mc.cores = nbCores)
  
  liste4 <- mclapply(1:p, FUN = one.simu_time_TSP,
                     data = matrix(runif(2*n), n, 2),
                     algo = "farthest",
                     mc.cores = nbCores)
  
  df[j ,] <- c("naif", n, do.call(cbind, liste1))
  df[j+1 ,] <- c("cheapest", n, do.call(cbind, liste2))
  df[j+2 ,] <- c("nearest", n, do.call(cbind, liste3))
  df[j+3 ,] <- c("farthest", n, do.call(cbind, liste4))
  j <- j + 4
}

df <- melt(df, id.vars = c("algo","n"))

############################
library(ggplot2) #ggplot
library(reshape2) #melt
library(parallel) #mclapply

data_summary <- function(data, varname, groupnames)
{
  require(plyr)
  summary_func <- function(x, col)
  {
    c(mean = mean(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], 0.01), q3 = quantile(x[[col]], 0.9))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- df
df2[,2] <- as.double(df[,2])
df2[,3] <- as.double(df[,3])
df2[,4] <- as.double(df[,4])
summary(df2)

df_new <- data_summary(df2, varname="value",
                       groupnames=c("algo","n"))

theMin <- min(df_new[,3:5],df_new[,3:5])
theMax <- max(df_new[,3:5],df_new[,3:5])

library(ggplot2)
ggplot(df_new, aes(x = n, y = value, col=algo)) +  scale_x_log10()+
  scale_y_log10(limits = c(theMin, theMax))  +
  labs(y = "time in seconds") +  labs(x = "number of cites") +
  geom_point(size = 2, aes(shape = algo)) +
  geom_errorbar(aes(ymin=`q1.1%`, ymax=`q3.90%`), width=.01) +
  scale_colour_manual(values = c("cheapest" = "#0080FF",
                                 "farthest" = " dark blue", "nearest" = "blue", "naif" = "red")) +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.text=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.position = c(0.1, 0.9),
        legend.title = element_blank())


th <- 25

R1 <- df_new[df_new$algo == "naif",c(2,3)]
R1 <- R1[th:dim(R1)[1],]
l1 <- lm(log(value) ~ log(n), data = R1, )
l1$coefficients

R2 <- df_new[df_new$algo == "cheapest",c(2,3)]
R2 <- R2[th:dim(R2)[1],]
l2 <- lm(log(value) ~ log(n), data = R2, )
l2$coefficients

R3 <- df_new[df_new$algo == "nearest",c(2,3)]
R3 <- R3[th:dim(R3)[1],]
l3 <- lm(log(value) ~ log(n), data = R3, )
l3$coefficients

R4 <- df_new[df_new$algo == "farthest",c(2,3)]
R4 <- R4[th:dim(R4)[1],]
l4 <- lm(log(value) ~ log(n), data = R4, )
l4$coefficients







