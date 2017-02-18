source("./base-funcs/maxsattsp_load_and_order_func.R")


#TSP
Mymodel.data <- LoadTsp("./modelresults/LM.tsp.pre/100percentleft/10_gompertzModelpositive_model.csv") 

NN.data <- read.csv("./modelresults/NN.tsp.pre/100percentleft/residuals_20iter_6hiddensize_50maxiter_1runs.csv") 

head(Mymodel.data)
resi.data <- select(Mymodel.data, instances, residuals) %>% cbind( NN.residuals = NN.data[, 2])

resi.data <- resi.data[order(resi.data$NN.residuals), ]


setEPS(width = 8, height = 6)
postscript("./proposal-paper/ANN-GPMP-part.eps")
plot(resi.data[1:100, 2], type = "l", lwd = 3, ylab = "", xlab = "")
lines(resi.data[1:100, 3],col = "red", lwd = 3, lty = 2)
title( ylab = "Resi.", xlab = "Index of Instances", cex.lab = 1.5)
legend("topleft", legend = c("GPMP", "ANN"), col = c("black", "red"), lty = c(1, 2), lwd = 2,cex = 1.5)
dev.off()

setEPS(width = 8, height = 6)
postscript("./proposal-paper/ANN-GPMP.eps")
plot(resi.data[, 2], type = "l", lwd = 3, ylab = "", xlab = "")
lines(resi.data[, 3],col = "red", lty = 2, lwd = 3)
title( ylab = "Resi.", xlab = "Index of Instances", cex.lab = 1.5)
legend("topleft", legend = c("GPMP", "ANN"), col = c("black", "red"), lty = c(1, 2), lwd = 2, cex = 1.5)
dev.off()


#####Maxsat


Mymodel.data <- LoadMaxsat("./modelresults/LM.maxsat.pre/100percentleft/10_logisticModelpositive_model.csv") 

NN.data <- read.csv("./modelresults/NN.maxsat.pre/100percentleft/residuals_20iter_6hiddensize_50maxiter_1runs.csv") 

head(Mymodel.data)
resi.data <- select(Mymodel.data, instances, residuals) %>% cbind( NN.residuals = NN.data[, 2])

resi.data <- resi.data[order(resi.data$NN.residuals), ]

resi.data <- resi.data[-grep("experiment", resi.data$instances), ]
setEPS(width = 8, height = 6)
postscript("./proposal-paper/ANN-LGMP-part.eps")
plot(resi.data[1:100, 2], type = "l", lwd = 3, ylab = "", xlab = "")
lines(resi.data[1:100, 3],col = "red", lwd = 3, lty = 2)
title(ylab = "Resi.", xlab = "Index of Instances", cex.lab = 1.5)
legend("topleft", legend = c("LGMP", "ANN"), col = c("black", "red"), lty = c(1, 2), lwd = 2,cex = 1.5)
grid(lwd = 2)
dev.off()

setEPS(width = 8, height = 6)
postscript("./proposal-paper/ANN-LGMP.eps")
plot(resi.data[, 2], type = "l", lwd = 3, ylab = "", xlab = "")
lines(resi.data[, 3],col = "red", lty = 2, lwd = 3)
title( ylab = "Resi.", xlab = "Index of Instances", cex.lab = 1.5)
grid(lwd = 2)
legend("topleft", legend = c("LGMP", "ANN"), col = c("black", "red"), lty = c(1, 2), lwd = 2, cex = 1.5)
dev.off()