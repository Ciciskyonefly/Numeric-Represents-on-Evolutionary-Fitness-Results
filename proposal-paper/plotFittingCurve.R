
rm(list = ls())
source("./base-funcs/plotCurve_logistic_single.R")
source("./base-funcs/ggplot2_theme_label.R")
data <- read.csv("./rawdata/maxsat/1FlipHCrs_uf100-09.csv")
theta <- c(0.170074442,56.31709022,0.69440227,-3.472325698)
index <- nrow(data)


setEPS(height = 4.5, width = 6)
names = paste("./proposal/ModelFitting-1.eps",sep = "")
postscript(names)

p <- plotCurve_exp_3_single(data, theta, index)

plot(p)

dev.off()


data2 <- read.csv("./rawdata/maxsat/1FlipHCrs_uf020-08.csv")
theta <- c(0.209739938, 13.44095382,0.76730645,-1.684255771)
index <- nrow(data2)


setEPS(height = 4.5, width = 6)
names = paste("./proposal/ModelFitting-2.eps",sep = "")
postscript(names)

p <- plotCurve_exp_3_single(data2, theta, index)

plot(p)

dev.off()