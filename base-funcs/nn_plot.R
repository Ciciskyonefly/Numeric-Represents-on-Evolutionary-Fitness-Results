
source("./base-funcs/ggplot2_theme_label.R")

NN_plot <- function(xData, yData, index){
  
  library(ggplot2)
  x <- xData
  y <- yData
  
  
  # if (index != length(xData)) {
  
  plot.dataframe  <- data.frame(x[seq(index)], y[seq(index)])
  names(plot.dataframe) <- c("plot.x", "plot.y")
  
  # p <- ggplot(plot.dataframe, aes(plot.x,plot.y)) +
  #         geom_point(colour = "blue", size = 1, stat = "identity") +
  #         scale_x_log10()
  
  plot(plot.dataframe$plot.x, plot.dataframe$plot.y,  log = "x", ylab = "Objective Value", 
       xlab = "FEs", pch = 20, cex = 1, col = "blue", ylim = c(min(y), max(y)), xlim = c(min(x), max(x)))
  grid()
  
  if(length(x[-seq(index)]) != 0){
    plot.dataframe1  <- data.frame(x[-seq(index)], y[-seq(index)])
    names(plot.dataframe1) <- c("x","y")
    # p <- p + geom_point(plot.dataframe1, mapping = aes(plot.dataframe1$x, plot.dataframe1$y), size = 1, colour = "red")
    points(plot.dataframe1$x, plot.dataframe1$y, col = "red")
    
  }
  
  
  
  
  
  #      } else {
  #useless in current work 2017_02_06
  # tag = floor(nrow(raw.data)/10)
  # 
  # for(i in 1:10){
  # 
  #         subscript <- c((tag*(i-1)+1):(tag*i))
  # 
  #         data.point <- data.frame(x[subscript],  y[subscript])
  #         names(data.point) <- c("x", "y")
  # 
  #         if(i == 1){
  #                 #
  #                 # p <- ggplot(data.point, aes(x, y)) +
  #                 #         geom_point(colour = i, stat = "identity")
  # 
  #                 # p <- ggplot(data.point, aes(x, y)) +
  #                 #         geom_point(colour = i, stat = "identity", size = 1) +
  #                 #         scale_x_log10()
  # 
  #                 plot(data.point$x, data.point$y,  log = "xy", ylab = "Objective Value", xlab = "FEs",
  #                      pch = 20, cex = 1, col = "blue", ylim(min(y), max(y)))
  #                 grid()
  #         } else {
  # 
  #                 #p <- p + geom_point(data.point, mapping = aes(x, y),colour = i, size = 1)
  #                 points(data.point$x, data.point$y, col = i, cex = 2)
  # 
  #         }
  # 
  # 
  # }
  # 
  # p
  
  #   }
  
}