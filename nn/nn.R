remove(list = ls())
# This .R file is used to choose the best maxiter of ANN .

library(neuralnet)
source("./data_preprocessing/readdata.R")
library(nnet)
library(dplyr)



datavolume = 0.5

qipa <- function(maxiter = 100,method = 1){
        
       TotalPath = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/maxSat/results/"
       pdfname <- paste(maxiter, "_testnnfit.pdf", sep = "")
       pdf(pdfname)
       par(mfrow = c(2, 2))
        firstFileList = list.files(TotalPath)
        firstFileList = firstFileList[-grep("\\.",firstFileList)]
        res.residuals <- NULL
        count.process = 0
        for(layer.j in 1:length(firstFileList)){
                firstFilePath = paste(TotalPath,firstFileList[layer.j],"/",sep = "")
                algorithm_name = paste(firstFileList[layer.j],"/",sep = "")
                a = list.files(firstFilePath)
                
                for(i in 1:length(a)){
                        if(grepl("\\.",a[i]) == TRUE) i= i+1
                        tryCatch({
                                instance_alogorithm_name = paste(algorithm_name,a[i],"/",sep = "")
                                pathpath = paste(firstFilePath,a[i],"/",sep = "")
                                data.matrix = getMatrixData(pathpath)
                                data.matrix = data.matrix[order(data.matrix[,1]), ]
                        },error = function(e){
                                
                        })
                        fixxData = data.matrix[ ,1]
                        fixyData = data.matrix[ ,3]
                        
                        if(method == 2){
                                sample.index = sort(sample(1:nrow(data.matrix), 
                                                           ceiling(nrow(data.matrix)*datavolume)))
                                
                        } else if (method == 1){
                                
                                sample.index = c(1:ceiling(nrow(data.matrix)*datavolume))
                        }
                        
                        x <- fixxData[sample.index]
                        y <- fixyData[sample.index]
                        
                        if( which(y == 0)%>% length() != 0 )
                                y[which(y == 0)] = min(y[-which(y == 0)])/2
                        #print(x)
                        #print(fixxData)
                        nn <- nnet(data.frame(x), data.frame(y), size= 4, maxit=maxiter, linout=TRUE, weights = 1/y, trace = FALSE)
                        #print (nn)
                        
                        
                        plot(fixxData, fixyData, log = 'x')
                        predict_y =  predict(nn, data.frame(fixxData), type = "raw")
                        
                        #print(nn$fitted.values)
                        lines( fixxData, predict_y, col = "2")
                        
                        
                        title(main = instance_alogorithm_name)
                        grid()
                        
                        residuals <- xyRMSE(y, nn$fitted.values) %>% print()
                        temp.residuals <- cbind(instance_alogorithm_name, residuals)
                        res.residuals <- rbind(res.residuals, temp.residuals)
                        
                        count.process = count.process + 1
                        print(paste("Process",count.process,"continue...",sep = ""))
                }
                
        }

        csv.name = paste(maxiter, "_testnnfit.csv", sep = "")
        write.csv(res.residuals, file = csv.name, row.names = FALSE)
        dev.off()
        
}




qipa(60)
qipa(100)
qipa(200)
qipa(300)
qipa(400)



