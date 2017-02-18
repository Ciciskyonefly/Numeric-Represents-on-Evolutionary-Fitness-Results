#the up-to-the-minute procedure,and the best one till now/2016/10/23
#rm(list = ls())
#source("../R_Code/core_fitting_procedure.R")

source("./base-funcs/readdata_func.R")

library(dplyr)


mainfunc <- function(PRE_OR_NOT = "not", maxTrain = 50, maxTest = 100, iter, list_model, file.path, save.path = "./BenchMarking/OPResults/MaxsatResult/", datavolume = 1, method = 1){
        # Return:
        #     Save function fitting "a", "b", "c", "d", "residuals"
        #
        # method: Sampling method. Keep Constant
        # PRE_OR_NOT : Run future of progress(Prediction) or not
        # maxTrain: In prediction, the maxFEs used for training data.[1, maxTrain]
        # maxTest: In prediction, the maxFEs used as testing data.Interval(maxTrain, maxTest]
        # list_model: list_models.
        # file.path: raw data path
        # save.path : results save path.
        # datavolume: The data used in this function, default all.
        # method: two ways of subsampling data. 1. Randomied choose. 2. Using the first part. #default 1
        count.process <- 0
        rawdata.path = file.path
        respath <-save.path
        relationship <- NULL
        eParaCsv <- NULL
        decayParaCsv <- NULL
        expLinearPara <- NULL
        gompertzParaCsv <- NULL
        if(!exists(save.path))
                dir.create(save.path)
        if(length(list_model) == 1){
          
          
          if(grepl("pre", PRE_OR_NOT)){
            pdfname <- paste(save.path, maxTrain, "_", maxTest, "_log_",iter,"_",unlist(list_model)$name,".pdf",sep = "")
          } else {
            pdfname <- paste(save.path, iter, "_", unlist(list_model)$name,".pdf",sep = "")
          }
          
        } else {
                if(grepl("pre", PRE_OR_NOT)){
                  pdfname <- paste(save.path,maxTrain, "_", maxTest, "_log_",iter,"_allmodel.pdf",sep = "")
                } else {
                  pdfname <- paste(save.path,"log_",iter,"_allmodel.pdf",sep = "")
                }
        }
        
        pdf(pdfname)
        par(mfrow = c(2, 2))
        
        instances.names <- rawdata.path %>% list.files()
        #list_plot <- list()
        ins <- 1
        
        all.parameters <- NULL
        for(ins in 1:length(instances.names)){
                
                instance.alogorithm.name <- gsub("_", "/", instances.names[ins])
                
                instance.alogorithm.name <- gsub(".csv", "", instance.alogorithm.name)
                pathpath <- paste(rawdata.path, instances.names[ins], sep = "")
                cat("pathpath: ", pathpath, "\n")
                if(grepl("pre", PRE_OR_NOT)){
                  
                  all.raw.data <- read.csv(pathpath)
                  #for train and prediction data.
                  test.script <- which(all.raw.data$x <= maxTest)
                  #   print(test.script)
                  if(length(test.script) !=0){
                    raw.data <- all.raw.data[c(1: max(test.script)), ]
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
                  if(method == 2){
                          sample.index = sort(sample(1:nrow(raw.data),ceiling(nrow(raw.data)*datavolume)))
                  } else if (method == 1){
                          sample.index = c(1:ceiling(nrow(raw.data)*datavolume))
                  }
                }
                
                xData <- raw.data$x[sample.index]
                yData <- raw.data$y[sample.index]
                
                #unseen data
                future.data.x <- raw.data$x[-sample.index]
                future.data.y <- raw.data$y[-sample.index]
                future.data <- data.frame(x = future.data.x, y = future.data.y)
                
                globalmodels <- list_model
                
                p <- findBestFitting(raw.data, xData, yData, globalmodels, iter)
                showplot <- p$plotFunction(raw.data, p$par, sample.index) 
                title(main = instance.alogorithm.name)
                count.process <- count.process + 1
                
                if(datavolume != 1){
                  if(!identical(p$par, c(0, 0, 0 ,0))){
                    predict_data <- GetFormulaValue(p$formula, future.data$x, p$par)
                    p$residual <- xyRMSE(future.data$y, predict_data[, 2])
                  }
                } else {
                  
                  if(!identical(p$par, c(0, 0, 0 ,0))){
                    predict_data <- GetFormulaValue(p$formula, raw.data$x, p$par)
                    p$residual <- xyRMSE(raw.data$y, predict_data[, 2])
                  }
                }
                
                temp.var <- cbind(instance.alogorithm.name, p$name)
                relationship <- rbind(relationship, temp.var, deparse.level = 0)
                temp_Para <- cbind(instance.alogorithm.name, p$name, t(p$par), p$residual, deparse.level = 0)
                
                all.parameters <- rbind(all.parameters, temp_Para, deparse.level = 0)
                
                if(length(globalmodels) == 1){
                        if(grepl("logisticModel",p$name))
                                eParaCsv = rbind(eParaCsv,temp_Para,deparse.level = 0)
                        if(grepl("gompertzModel",p$name))
                                gompertzParaCsv = rbind(gompertzParaCsv,temp_Para,deparse.level = 0)
                        if(grepl("decay",p$name)){
                                decayParaCsv = rbind(decayParaCsv,temp_Para,deparse.level = 0)
                        }
                        if(grepl("expLinearModel",p$name))
                                expLinearPara = rbind(expLinearPara,temp_Para,deparse.level = 0)
                }
                
            #    count.process = count.process + 1
                if(count.process %% 1000 == 0){
                  print(paste("Process ",count.process," continue...",sep = ""))
                }
                
        }
        
        
        dev.off()
        
        #ggplot2 save. Cost too much time.
        # library(gridExtra)
        # ggsave(filename = pdfname, marrangeGrob(grobs = list_plot, nrow = 2, ncol = 2))
      
        if(length(globalmodels) != 1){
          colnames(all.parameters) <- c("instance_file","model","a","b","c","d","residuals")
          allname.log <- paste(respath, iter, "_all_model.csv", sep = "")
          write.csv(all.parameters, allname.log, row.names = FALSE, col.names = TRUE)
        }
        
        # if(!is.null(relationship)){
        #         colnames(relationship) <- c("instance_file","model")
        #         csvname.rela <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep="")
        #         write.csv(relationship,csvname.rela,row.names = FALSE,col.names = TRUE)
        # }
        # 
        
        if(length(globalmodels) == 1){
                if(!is.null(eParaCsv)){
                        colnames(eParaCsv) <-  c("instance_file","model","a","b","c","d","residuals")
                        if(grepl("pre", PRE_OR_NOT)){
                          csvname.log <- paste(respath,maxTrain,"_", maxTest, "_", iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        } else {
                          csvname.log <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        }
                        write.csv(eParaCsv,csvname.log,row.names = FALSE,col.names = TRUE)
                }
                
                
                if(!is.null(expLinearPara)){
                        colnames(expLinearPara) <- c("instance_file","model","a","b","c","d","residuals")
                        if(grepl("pre", PRE_OR_NOT)){
                          csvname.linear <- paste(respath,maxTrain,"_", maxTest, "_", iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        } else {
                          csvname.linear <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        }
                        write.csv(expLinearPara,csvname.linear,row.names = FALSE,col.names = TRUE)
                }
                
                if(!is.null(decayParaCsv)){
                        colnames(decayParaCsv) <-  c("instance_file","model","a","b","c","d","residuals")
                        if(grepl("pre", PRE_OR_NOT)){
                          csvname.decay <- paste(respath,maxTrain,"_", maxTest, "_", iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        } else {
                          csvname.decay <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        }
                        write.csv(decayParaCsv,csvname.decay,row.names = FALSE,col.names = TRUE)
                }
                
                
                if(!is.null(gompertzParaCsv)){
                        colnames(gompertzParaCsv)<-  c("instance_file","model","a","b","c","d","residuals")
                        if(grepl("pre", PRE_OR_NOT)){
                          csvname.gomp <- paste(respath,maxTrain,"_", maxTest, "_", iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        } else {
                          csvname.gomp <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                        }
                        write.csv(gompertzParaCsv,csvname.gomp,row.names = FALSE,col.names = TRUE )
                }
        }
  
        
}

