
rm(list = ls())
source("./base-funcs/plotCurve_logistic_single.R")
source("./base-funcs/ggplot2_theme_label.R")
data <- read.csv("./rawdata/maxsat/1FlipHC_uf150-07.csv")




x60 <- c(0.094016855, 76.60737749, 1.116399406, -5.26050743)
x70 <- c(2.497110346,	74.08350766,	1.140577549,	-5.313877291)
x80 <- c(4.615239903,	71.62895698,	1.18480766,	-5.464010963)
x90 <- c(1.85415819,	75.28946447,	1.100106337,	-5.130030137)
x100 <- c(1.516179998,	75.98536759,	1.080141571,	-5.035258825)


para.matrix <- rbind(x60, x70, x80, x90, x100)

j = 1
for(i in seq(0.6, 1, 0.1)){
        
        index = ceiling(nrow(data)*i)
        theta <- para.matrix[j, ]
        j = j+1
       # debug(plotCurve_exp_3_single)
        setEPS(height = 4.5, width = 6)
        names = paste("./proposal/performance-prediction-log-",100*i, ".eps",sep = "")
        postscript(names)
        p <- plotCurve_exp_3_single(data, theta, index)
        plot(p)
        dev.off()
}