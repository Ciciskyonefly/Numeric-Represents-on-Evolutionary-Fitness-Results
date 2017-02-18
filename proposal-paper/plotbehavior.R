#####plot data point file

Myggolot2.label <- function(){
        
        labs(
            #    x = "FEs",
            #    y = "Fitness Value",
                x = "Runtime",
                y = "Objective Value",
                colour = "Cylinders"
        ) 
        
}

Myggolot2.theme <- function(){
        
        
        theme( 
                #   plot.title = element_text(size=22),
                #   size = 0.5,
                panel.grid.major = element_line( linetype = 3 , colour = "lightgrey" ),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black" ),
        #        axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
        #        axis.title.y=element_blank(),
                axis.title=element_text(size=17,face="bold"),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
                #      axis.text.x = element_text(size = 15, hjust = 1),
                #      axis.text.y = element_text(size = 15, hjust = 1)
        )
}


library(ggplot2)
#data <- read.csv("./rawdata/maxsat/1FlipHCrs_uf100-09.csv")
data <- read.csv("./uf250-05_mFlipHCrs_20.txt", sep = "\t")
colnames(data) <- c("x", "nt", "y")
data <- data[, c(1,3)]
#index = sample(1 : nrow(data), 60) %>% sort
#data = data[index, ]
# setEPS(height = 6, width = 8)
# names = paste("./paper-figure/figure-1/anytime-behavior.eps",sep = "")
# postscript(names)

p <- ggplot(data, aes(x, y)) +
        geom_point(colour = "blue", stat = "identity", size = 2) + scale_x_log10()
      
p <- p + Myggolot2.theme()

p <- p+  Myggolot2.label()
plot(p)

#dev.off()
