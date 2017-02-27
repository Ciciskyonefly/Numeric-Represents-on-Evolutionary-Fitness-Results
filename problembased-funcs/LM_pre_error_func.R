rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/path_config.R")
source("./base-funcs/models_func.R")

recalculate_error_func <- function(file, problem,  old.maxTest, maxTe){
    
    res.dat <- read.csv(file)
    rawdata.path <- path_config(problem)[1]
    restore.path <- path_config(problem)[2]
    
    tmp.string <- strsplit(file, "_") %>% unlist()
    maxTrain <-  tmp.string[2]%>%as.numeric()
    cat("maxTrain: ", maxTrain, "\n")
    for(maxtest in 1:length(maxTe)){
        maxTest <- maxTe[maxtest]
        for(fi in 1:nrow(res.dat)){
            
            rawdata.name <- gsub("/", "_", res.dat[fi, "instance_file"] %>% as.character)
            rawdata <- paste(rawdata.path, rawdata.name, ".csv", 
                             sep = "") %>% read.csv()
            
            test.script <- which(rawdata$x <= maxTest)
            if(length(test.script) !=0){
                raw.data <- rawdata[c(1: max(test.script)), ]
            } else {
                next
            }
            #train data
            train.script <- which(rawdata$x <= maxTrain)
            if(length(train.script) != 0){
                sample.index <- c(1: max(train.script))
            } else {
                next
            }
            
            
            xData <- raw.data$x[sample.index]
            yData <- raw.data$y[sample.index]
            train.data <- data.frame(x = xData, y = yData)
            #unseen data
            
            future.data.x <- raw.data$x[-sample.index]
            future.data.y <- raw.data$y[-sample.index]
            future.data <- data.frame(x = future.data.x, y = future.data.y)
            
            par <- res.dat[fi, c("a", "b", "c", "d")] %>% as.numeric
            if(nrow(future.data) != 0){
                tmp.pre_y <- GetFormulaValue(eval(parse(text = res.dat$model[fi] %>% as.character))$formula, future.data$x, par)[, 2]
                residual = xyRMSE(future.data$y, tmp.pre_y)
              #  cat(residual, "...fi:", fi, "\n")
                res.dat[fi, "residuals"] <- residual
            }
        }
        
        
        library(stringr)
        restore.res.dat.csvname <- gsub(paste(maxTrain,"_", old.maxTest, "_", sep = ""), paste(maxTrain,"_", maxTest, "_", sep = ""), file)
        write.csv(res.dat, file = restore.res.dat.csvname, row.names = FALSE)
        
    }
    
}  

library(dplyr)
path <- "./modelresults/LM.maxsat.pre/maxTrain-maxTest/"
res.list <- list.files(path)
res.list <- paste(path, res.list[grep("C_.+.csv", res.list)], sep = "")
for(lm in 1:length(res.list)){
    recalculate_error_func(res.list[lm], problem = "maxsat", old.maxTest = 100,  maxTe = c(100, 1000, 10000))
}

