#the up-to-the-minute procedure,and the best one till now/2016/10/23
#rm(list = ls())
#source("../R_Code/core_fitting_procedure.R")

source("./baseFuncs/readdata_func.R")
source("./baseFuncs/nn_plot.R")
source("./baseFuncs/models_func.R")
library(ggplot2)
library(dplyr)
library(data.table)
library(nnet)

NN <- function(xData, yData, sample.index, iter, maxiter, hiddensize){
        
        result <- NULL
        x <- xData[sample.index]
        y <- yData[sample.index]
        
        
        if( which(y == 0)%>% length() != 0 )
                y[which(y == 0)] = min(y[-which(y == 0)])/2
        
        sum_y = rep(0, length(yData))
        residuals = 0
        for( i in 1 : iter){
                nn <- nnet(data.frame(x), data.frame(y), size= hiddensize, maxit=maxiter, linout=TRUE, weights = 1/y, trace = FALSE)
                predict_y =  predict(nn, data.frame(xData), type = "raw")
                temp.residuals = xyRMSE(yData, predict_y)
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


tsp.mainfunc <- function(maxiter = 50, hiddensize = 4, iter,file_path,save_path = "./BenchMarking/OPResults/MaxsatResult/",datavolume = 1,method = 1, runtime = 1){
        
        TotalPath = file_path
        respath = save_path
        for(run in 1:runtime){
                
                algorithm.listnames = list.files(TotalPath)
                n = length(algorithm.listnames)
                
                save.wts <- NULL
                save.residuals <- NULL
                save.both <- NULL
                
                pdfname = paste(save_path,iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.pdf",sep = "" )
                list_plot = list()
                pred_y = NULL
                count.process = 0
                #######################
                #save modeling curve  #
                #######################
                
                
                for(i in 1:n){
                        
                        instance.filepath = paste(TotalPath,algorithm.listnames[i], "/", "symmetric", '/', sep = "")
                        
                        instance.listnames = list.files(instance.filepath)
                        m = length(instance.listnames)
                        for(j in 1:m){#
                                
                                data.filepath = paste(instance.filepath,instance.listnames[j],sep = "")
                                instance_alogorithm_name = paste(algorithm.listnames[i], "/symmetric/", instance.listnames[j], sep = "")
                                
                                s = getTheMatrixData_TspSuite(data.filepath)
                                s = s[order(s[, 1]), ]
                                xData = s[, 1]
                                yData = s[, 3]
                                
                                
                                if(method == 2){
                                        sample.index = sort(sample(1:nrow(s), ceiling(nrow(s)*datavolume)))
                                        
                                } else if (method == 1){
                                        
                                        sample.index = c(1:ceiling(nrow(s)*datavolume))
                                }
                                
                                
                                p <- NN(xData, yData, sample.index, iter, maxiter, hiddensize)
                                
                                showplot <- NN_plot(xData, yData, sample.index)
                                pre_y <- p$pre_y
                                data.line = data.frame(xData, pre_y)
                                names(data.line) <- c("x", "y")
                                showplot <- showplot + geom_line (data.line, mapping = aes(x, y), col = "green", size = 0.8)
                                showplot <- showplot + Myggolot2.theme() + Myggolot2.label() + ggtitle(instance_alogorithm_name)
                                
                                plot(showplot)
                                count.process =  count.process + 1
                                
                                list_plot[[count.process]] <- showplot  
                                
                                save.wts <- cbind(instance_alogorithm_name, t(p$wts)) %>% rbind(save.wts)
                                save.residuals <-  cbind(instance_alogorithm_name, p$residuals) %>% rbind(save.residuals)
                                save.both <- cbind(instance_alogorithm_name, t(p$wts), p$residuals) %>% rbind(save.both)
                                
                        }
                        
                        print ("ok")
                }
                
                library(gridExtra)
                ggsave(filename = pdfname,  marrangeGrob(grobs = list_plot, nrow = 2, ncol = 2))
                
                write.csv(save.wts, file = paste(save_path, "wts_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv",sep = ""),  row.names = FALSE)
                write.csv(save.residuals, file = paste(save_path, "residuals_", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
                write.csv(save.both, file = paste(save_path, "_wts_residuals", iter, "iter_", hiddensize, "hiddensize_" , maxiter, "maxiter_", run,"runs.csv", sep = ""),  row.names = FALSE)
        }
       
        
}

