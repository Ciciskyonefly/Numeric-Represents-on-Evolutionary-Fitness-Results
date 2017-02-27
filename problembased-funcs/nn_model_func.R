#the up-to-the-minute procedure,and the best one till now/2016/10/23
#rm(list = ls())
#source("../R_Code/core_fitting_procedure.R")

source("./base-funcs/readdata_func.R")
source("./base-funcs/nn_plot.R")
source("./base-funcs/models_func.R")
library(ggplot2)
library(dplyr)
library(data.table)
library(nnet)



NN <- function(xData, yData, sample.index, iter, maxiter, hiddensize){
    
    result <- NULL
    x <- xData[sample.index]
    y <- yData[sample.index]
    
    future.data.x <- xData[-sample.index]
    future.data.y <- yData[-sample.index]
    future.data <- data.frame(x = future.data.x, y = future.data.y)
    
    
    sum_y = rep(0, length(yData))
    residuals = 0
    for( i in 1 : iter){
        #, weights cannot smaller than 0.  weights = 1/y, 
        nn <- nnet(data.frame(x), data.frame(y),  size = hiddensize, maxit= maxiter, weights = weights_funcs(y), linout = TRUE,  trace = FALSE)
        if(nrow(future.data) != 0){
            predict_y =  predict(nn, data.frame(future.data.x), type = "raw")
          #  cat("length predict_y, future.data.y:", length(predict_y), length(future.data.y), "\n")
            temp.residuals = xyRMSE(future.data.y, predict_y)
            #for print line
            predict_y =  predict(nn, data.frame(xData), type = "raw")
        } else {
            predict_y =  predict(nn, data.frame(xData), type = "raw")
            temp.residuals = xyRMSE(yData, predict_y)
        }
        
        if(i == 1){
            residuals <- temp.residuals
        } 
        
        if(temp.residuals <= residuals){
            result$pre_y <- predict_y
            result$wts <- nn$wts
            result$residuals <- temp.residuals
        }
        
    }
    
    return (result)
}


NN.mainfunc <- function(PRE_OR_NOT = "not", maxTrain = 50, maxTest = 100, maxiter = 50, hiddensize = 6, iter, file.path, save.path = "./BenchMarking/OPResults/MaxsatResult/",datavolume = 1,method = 1, runtime = 1){
    # Function for Artificial Neutral Network fitting curve.
    # Args:
    #   cal.error.method : Sampling  cal.error.method . Keep Constant
    #   PRE_OR_NOT : Run future of progress(Prediction) or not
    #   maxTrain : In prediction, the maxFEs used for training data.[1, maxTrain]
    #   maxTest : In prediction, the maxFEs used as testing data.Interval(maxTrain, maxTest]
    #   file.path : raw data path
    #   save.path : results save path.
    #   cal.error.method : two ways of subsampling data. 1. Randomied choose. 2. Using the first part. #default 1
    #   Runtime : How many times this NN fitting functions work. 
    # Return:
    #   Save ANN approximator fitting resing  "residuals", "weights"
    for(run in 1:runtime){
        
        rawdata.path = file.path
        instances.names <- rawdata.path %>% list.files()
        save.wts <- NULL
        save.residuals <- NULL
        save.both <- NULL
        
        pdfname = paste(save.path, maxTrain,"_", maxTest, "_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.pdf",sep = "" )
        pdf(pdfname)
        par(mfrow = c(2, 2))
        # list_plot = list()
        pred_y = NULL
        count.process = 0
        for(ins in 1:length(instances.names)){
            
            instance.alogorithm.name <- gsub("_", "/", instances.names[ins])
            instance.alogorithm.name <- gsub(".csv", "", instance.alogorithm.name)
            pathpath <- paste(rawdata.path, instances.names[ins], sep = "")
       #     print(pathpath)
            #for train and prediction data.
            if(grepl("pre", PRE_OR_NOT)){
                all.raw.data <- read.csv(pathpath)
                test.script <- which(all.raw.data$x <= maxTest)
                if(length(test.script) !=0){
                    raw.data <-all.raw.data[c(1: max(test.script)), ]
                } else {
                    next
                }
                train.script <- which(raw.data$x <= maxTrain)
                if(length(train.script) != 0){
                    sample.index <- c(1: max(train.script))
                } else {
                    next
                }
                
            } else {
                raw.data <- read.csv(pathpath)
                sample.index = c(1:ceiling(nrow(raw.data)*datavolume))
            }
            
            xData = raw.data$x
            yData = raw.data$y
            
            p <- NN(xData, yData, sample.index, iter, maxiter, hiddensize)
            #omit plot
            showplot <- NN_plot(xData, yData, sample.index)
            pre_y <- p$pre_y
            data.line = data.frame(xData, pre_y)
            names(data.line) <- c("x", "y")
            lines(data.line$x, data.line$y, col = "green", lwd = 2)
            title(main = instance.alogorithm.name)
            
            # ggplot theme
            # showplot <- showplot + geom_line (data.line, mapping = aes(x, y), col = "green", size = 0.8)
            # showplot <- showplot + Myggolot2.theme() + Myggolot2.label() + ggtitle(instance.alogorithm.name) + coord_fixed(ratio = 0.75)
            # #     plot(showplot)
            count.process =  count.process + 1
            # 
            # list_plot[[count.process]] <- showplot  
            # 
            # 
            save.wts <-  rbind(save.wts, cbind(instance.alogorithm.name, t(p$wts)))
            save.residuals <-  rbind(save.residuals, cbind(instance.alogorithm.name, p$residuals))
            save.both <- rbind(save.both, cbind(instance.alogorithm.name, residuals = p$residuals, t(p$wts)))
            
            if(count.process %% 1000 == 0)
                print(count.process)
            
        }
   #     cat("all run: ", count.process, " \n")
        print ("ok")
    }
    
    dev.off()
    # library(gridExtra)
    # ggsave(filename = pdfname, marrangeGrob(grobs = list_plot, nrow = 2, ncol = 2))
    # 
    
    if(grepl("pre", PRE_OR_NOT)){
        write.csv(save.wts, file = paste(save.path,"pre_wts_", maxTrain,"_", maxTest,"_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv",sep = ""),  row.names = FALSE)
        write.csv(save.residuals, file = paste(save.path, "pre_resi_", maxTrain,"_", maxTest, "_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
        write.csv(save.both, file = paste(save.path, "pre_wts_resi,", maxTrain,"_", maxTest, "_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
    } else {
        write.csv(save.wts, file = paste(save.path, "not_pre_wts", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv",sep = ""),  row.names = FALSE)
        write.csv(save.residuals, file = paste(save.path, "not_pre_resi", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
        write.csv(save.both, file = paste(save.path,"not_pre_wts_resi", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
    }
    
}


