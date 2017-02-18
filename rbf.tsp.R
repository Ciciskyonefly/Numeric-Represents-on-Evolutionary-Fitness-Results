# source("./models_var.R")
# source("./models_func.R")
library(dplyr)
source("./data_preprocessing/readdata.R")
library(neuralnet)
source("./data_preprocessing/readdata.R")
library(nnet)


xyRMSE <- function(y, pred.y) {
        
        square.residuals <- (y - pred.y) * (y - pred.y)
        
        if( which(y == 0)%>% length() != 0 )
                y[which(y == 0)] = min(y[-which(y == 0)])/2
        
        eva.residuals <- sum(square.residuals/y)
        
}


file.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/ts0r0b/symmetric/"

list.names <- list.files(file.path)

par(mfrow = c(1, 2))
for(i in 1:length(list.names)){
        
        every_path = paste(file.path, list.names[i], sep = "")
        data.matrix = getTheMatrixData_TspSuite(every_path)
        head(data.matrix)
        data.matrix = data.matrix[order(data.matrix[, 1]), ]
        #data.matrix = data.matrix[sample.index,]
        
        x = data.matrix[, 1]
        y = data.matrix[, 3]
        x <- x %>% matrix(ncol = 1)
        y <- y %>% matrix(ncol = 1)
        data<-rbftrain(x,neurons,y,sigma=NaN, visual = FALSE)
        
        rbfn <- rbf(x,data$weight,data$dist,data$neurons,data$sigma)
        
        plot(x,y, log = "x")
        lines(x,rbfn, col = "red")
        grid()
        
        
        
        x = data.matrix[, 1]
        y = data.matrix[, 3]
        plot(x, y, log = 'x')
        maxiter = 400
        if( which(y == 0)%>% length() != 0 )
                y[which(y == 0)] = min(y[-which(y == 0)])/2
        #print(x)
        #print(fixxData)
        nn <- nnet(data.frame(x), data.frame(y), size= 4, maxit=maxiter, linout=TRUE, weights = 1/y, trace = FALSE)
        #print (nn)
        
        
        
        predict_y =  predict(nn, data.frame(x), type = "raw")
        
        #print(nn$fitted.values)
        lines( x, predict_y, col = "2")
        
        
        grid()
        
        residuals <- xyRMSE(y, nn$fitted.values) %>% print()
        
        
}



#rbf(x,data$weight,data$dist,data$neurons,data$sigma)
# xData <- fixxData
# yData <- fixyData
# xpara <- getInitialParameters(gompertzModelpositive, xData, yData)
# nls.res <- nlsSolverSingleModel(gompertzModelpositive, xData, yData)
# nls.res
# print(nls.res)
# #debug(gompertzModelpositive$plotFunction)
# 
# setEPS()
# postscript("showggplot.eps")
# q <- gompertzModelpositive$plotFunction(data.matrix, nls.res$par,  nrow(data.matrix)) + ggtitle("ok")
# plot(q)
# dev.off()