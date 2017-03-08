#the up-to-the-minute procedure,and the best one till now/2016/10/23
#rm(list = ls())
#source("../R_Code/core_fitting_procedure.R")

source("./base-funcs/readdata_func.R")

library(dplyr)


mainfunc <- function(PRE_OR_NOT = "not", maxTrain = 50, maxTest = 100, iter, list_model, file.path, save.path = "./BenchMarking/OPResults/MaxsatResult/", cal.error.method = "BL"){
    # Return:
    #     Save function fitting "a", "b", "c", "d", "residuals"
    #
    # cal.error.method : Sampling  cal.error.method . Keep Constant
    # PRE_OR_NOT : Run future of progress(Prediction) or not
    # maxTrain: In prediction, the maxFEs used for training data.[1, maxTrain]
    # maxTest: In prediction, the maxFEs used as testing data.Interval(maxTrain, maxTest]
    # list_model: list_models.
    # file.path: raw data path
    # save.path : results save path.
    # datavolume: The data used in this function, default all.
    
    if(!exists(save.path))
        dir.create(save.path)
    if(length(list_model) == 1){
        #single
        if(grepl("pre", PRE_OR_NOT)){
            pdfname <- paste(save.path, cal.error.method, "_", maxTrain, "_", maxTest, "_log_",iter,"_",unlist(list_model)$name,".pdf",sep = "")
            csv.name <-  paste(save.path, cal.error.method, "_", maxTrain, "_", maxTest, "_log_", iter, "_", unlist(list_model)$name,".csv",sep = "")
        } else {
            pdfname <- paste(save.path, iter, "_", unlist(list_model)$name,".pdf",sep = "")
            csv.name <- paste(save.path, iter, "_", unlist(list_model)$name,".csv",sep = "")
        }
        
    } else {
        #all model
        if(grepl("pre", PRE_OR_NOT)){
            pdfname <- paste(save.path, maxTrain, "_", maxTest, "_log_",iter,"_all_model.pdf",sep = "")
            csv.name <- paste(save.path, maxTrain, "_", maxTest, "_log_",iter,"_all_model.csv",sep = "")
        } else {
            pdfname <- paste(save.path,"log_", iter, "_all_model.pdf", sep = "")
            csv.name <- paste(save.path, "log_", iter, "_all_model.csv", sep = "")
        }
    }
    cat("hello\n")
    pdf(pdfname)
    par(mfrow = c(2, 2))
    
    instances.names <- file.path %>% list.files()
    #list_plot <- list()
    ins <- 1
    all.parameters <- NULL
    for(ins in 1:length(instances.names)){
        
        instance.alogorithm.name <- gsub("_", "/", instances.names[ins])
        instance.alogorithm.name <- gsub(".csv", "", instance.alogorithm.name)
        pathpath <- paste(file.path, instances.names[ins], sep = "")
        
   
        if(grepl("pre", PRE_OR_NOT)){
            
            all.raw.data <- read.csv(pathpath)
            
            #for train and prediction data.
            #test data
            test.script <- which(all.raw.data$x <= maxTest)
            if(length(test.script) !=0){
                raw.data <- all.raw.data[c(1: max(test.script)), ]
            } else {
                next
            }
            #train data
            train.script <- which(raw.data$x <= maxTrain)
            if(length(train.script) != 0){
                sample.index <- c(1: max(train.script))
            } else {
                next
            }
            
        } else {
            raw.data <- read.csv(pathpath)
            sample.index = c(1:ceiling(nrow(raw.data)))
        }
        
        xData <- raw.data$x[sample.index]
        yData <- raw.data$y[sample.index]
        train.data <- data.frame(x = xData, y = yData)
        #unseen data
        future.data.x <- raw.data$x[-sample.index]
        future.data.y <- raw.data$y[-sample.index]
        future.data <- data.frame(x = future.data.x, y = future.data.y)
        
        globalmodels <- list_model
        
        if(grepl("BL", cal.error.method)){
            p <- findBestFitting(train.data, xData, yData, globalmodels, iter)
        }
        
        p$plotFunction(raw.data, p$par, sample.index) 
        title(main = instance.alogorithm.name)
        tmp.para <- cbind(instance.alogorithm.name, p$name, t(p$par), p$residual, deparse.level = 0)
        all.parameters <- rbind(all.parameters, tmp.para, deparse.level = 0)
        
    }
    
 

    dev.off()
    cat("sleep")
    cat(csv.name, "\n")
    colnames(all.parameters) <- c("instance_file","model","a","b","c","d","residuals")
    write.csv(all.parameters, csv.name, row.names = FALSE, col.names = TRUE)
    
  
    
}




