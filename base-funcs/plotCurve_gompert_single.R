

source("./base-funcs/ggplot2_theme_label.R")

plotCurve_gompert_single <- function(s,theta,index = nrow(s)){
       
        library(ggplot2)
        
        x = s$x
        y = s$y

     #   cat("theta:", theta, "\n")
        
        y1 = theta[1]+ theta[2]*exp(theta[3]*exp(x*theta[4])) 
        # if(is.infinite(y1) %>% which() %>% length() != 0) {
        #         y1[is.infinite(y1) %>% which()] = 0
        # }
        
       if (index != nrow(s)) {

                plot.dataframe  <- data.frame(x[seq(index)], y[seq(index)])
                names(plot.dataframe) <- c("plot.x", "plot.y")

                # p <- ggplot(plot.dataframe, aes(plot.x,plot.y)) +
                #         geom_point(colour = "blue", stat = "identity") + 
                #         scale_x_log10() + scale_y_log10()
                #      ylim(min(y), max(y))
                
                plot(plot.dataframe$plot.x, plot.dataframe$plot.y,  log = "x", ylab = "Objective Value", 
                     xlab = "FEs", pch = 20, cex = 1, col = "blue", ylim = c(min(y), max(y)), xlim = c(min(x), max(x)))
                grid()
                

                # plot(xPercent,  yPercent,
                #      xlab = "FEs",  ylab = "F",
                #      pch = 6, col = "red",
                #      xlim = c(min(x),max(x)), ylim = c(min(y),max(y)))


               if(length(x[-seq(index)]) != 0){
                        plot.dataframe1  <- data.frame(x[-seq(index)], y[-seq(index)])
                        names(plot.dataframe1) <- c("x","y")
                        #p <- p + geom_point(plot.dataframe1, mapping = aes(plot.dataframe1$x, plot.dataframe1$y), colour = "red")
                        points(plot.dataframe1$x, plot.dataframe1$y, colour = "red")
               }
                

                 data.line = data.frame(x,y1)
                # p <- p + geom_line (data.line, mapping = aes(x, y1), col = "green", size =0.8)
                # p <- p + Myggolot2.theme() + Myggolot2.label()
                # p
                 lines(data.line$x, data.line$y1, col = "green", lwd = 2)
                 

        } else {

                tag = floor(nrow(s)/10)

                for(i in 1:10){

                        subscript <- c((tag*(i-1)+1):(tag*i))

                        data.point <- data.frame(x[subscript],  y[subscript])
                        names(data.point) <- c("x", "y")
                        
                        if(i == 1){
                                # p <- ggplot(data.point, aes(x, y)) +
                                #         geom_point(colour = i, stat = "identity", size = 2) +
                                #         scale_x_log10()
                                        
                                plot(data.point$x, data.point$y,  log = "x", ylab = "Objective Value", xlab = "FEs",
                                     pch = 20, cex = 1, col = "blue", ylim(min(y), max(y)))
                                grid()
                                
                                
                        } else {
                                
                                #        p <- p + geom_point(data.point, mapping = aes(x, y), col = i, size = 2)
                                points(data.point$x, data.point$y, col = i, cex = 2)
                                
                        }

        
                }
                
                data.line = data.frame(x,y1)
                # p <- p + geom_line (data.line, mapping = aes(x, y1), col = "#FF6633", size = 1)
                # p <- p + Myggolot2.theme() + Myggolot2.label()
                # p
                lines(data.line$x, data.line$y1, col = "#FF6633", lwd = 2)

        }

        
}

