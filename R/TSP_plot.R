

plot.TSP <- function(tour, data, value = "")
{
  plot(data[,1], data[,2], 
       xlim = c(min(data[,1]), max(data[,1])),
       ylim = c(min(data[,2]), max(data[,2])), 
       xlab = "", ylab = "", main = "", asp = 1, xaxt = "n", yaxt = "n")
  n <- length(tour)
  col_fun <- colorRamp(c("blue", "red"))
  rgb_cols <- col_fun(1:n / n)
  cols <- rgb(rgb_cols, maxColorValue = 256)
  for(i in 1:(n-1)){segments(x0 = data[tour[i],1],
                                        y0 = data[tour[i],2],
                                        x1 = data[tour[i+1],1],
                                        y1 = data[tour[i+1],2], lwd = 1, col = cols[i])}
  segments(x0 = data[tour[length(tour)],1],
           y0 = data[tour[length(tour)],2],
           x1 = data[tour[1],1],
           y1 = data[tour[1],2], lwd = 1, col = cols[n])
  title(sub = value, adj = 1, line = 0.5, font = 4)
}



tour_length <- function(tour, villes)
{
  return(sum(apply(apply(villes[c(tour, tour[1]),],2,function(x) diff(x)^2), 1 ,function(x) sqrt(sum(x)))))
}






