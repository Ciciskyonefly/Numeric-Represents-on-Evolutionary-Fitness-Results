remove(list = ls())
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
library(dplyr)
predict.path <- "./modelresults/LM.tsp.pre/singleRun/maxTrain-maxTest/"
rawdata.path <- "./rawdata/tsp-singleRun/"
dat.list <- predict.path %>% list.files()
dat.list <- dat.list[-grep("all_model", dat.list)]

maxTrain <- c(10, 50, 100)
maxTest <- c(100, 1000, 10000)

train <- 1
test <- 1

#TODO

all.error.res <- NULL
all.error.res.colnames <- c("instance_file")
all.error.res.csvname<- paste("./modelresults/LM.tsp.pre/singleRun/", "all_train_test_weighted_merge_yData_error.csv", sep = "")
for(train in 1:length(maxTrain)){
    for(test in 1:length(maxTest)){
        pdfname <- paste("./modelresults/LM.tsp.pre/singleRun/", maxTrain[train], "_", maxTest[test], "weighted_merge_curve.pdf", sep = "")
        pdf(pdfname)
        par(mfrow= c(2,2))
        pattern <- paste(maxTrain[train], "_", maxTest[test],"_.+.csv", sep = "")
        Train.Test.dat.list <- paste(predict.path, dat.list[grep(pattern, dat.list)], sep = "") 
        
        #all dat in every single maxTrain and maxTest couple
        all.dat <- NULL
        for(dat in 1:length(Train.Test.dat.list)){
            cat(Train.Test.dat.list[dat], "\n")
            all.dat <- all.dat %>% rbind(LoadTsp(Train.Test.dat.list[dat]))
        }
        dat.split <- all.dat %>% split(all.dat$instance_file, all.dat$model, drop = TRUE) 
        split <- 1
        
        error.res <- NULL
        for(split in 1:length(dat.split)){
            
            tmp.dat <- dat.split[[split]][order(dat.split[[split]]$residuals), ]
            
            if( which(tmp.dat$residuals == 1000000) %>% length() != 0)
                tmp.dat <- tmp.dat[-which(tmp.dat$residuals == 1000000), ]
            
            if(nrow(tmp.dat) == 0) next
            
            rawdata.name <- gsub("/", "_", tmp.dat[1, "instance_file"] %>% as.character)
            rawdata <- paste(rawdata.path, rawdata.name, ".csv", 
                             sep = "") %>% read.csv()
            
            xData <- rawdata$x[which(rawdata$x < maxTest[test])]
            yData <- rawdata$y[which(rawdata$x < maxTest[test])]
            train.xData <- rawdata$x[which(rawdata$x < maxTrain[train])]
            train.yData <- rawdata$y[which(rawdata$x < maxTrain[train])]
            
            weight_yData <- NULL
            weighted <- tmp.dat$residuals/sum(tmp.dat$residuals)
            for(j in 1:nrow(tmp.dat)){
                par <- tmp.dat[j, c("a", "b", "c", "d")] %>% as.vector %>% as.numeric
                if(is.null(weight_yData)){
                    weight_yData <- weighted[nrow(tmp.dat) - j + 1] * eval(parse(text = tmp.dat$model[j] %>% as.character))$modelFunction(par, xData)
                } else {
                    weight_yData <- weight_yData + weighted[nrow(tmp.dat) -j + 1]*eval(parse(text = tmp.dat$model[j] %>% as.character))$modelFunction(par, xData)
                }
            }
            
            
            tmp.error.res <- cbind(tmp.dat[1, "instance_file"] %>% as.character, xyRMSE(yData, weight_yData))
            if(is.null(error.res)){
                error.res <- tmp.error.res
            } else {
                error.res <- error.res %>% rbind(tmp.error.res)
            }
            
            plot(xData, yData, pch = 20, log = "x", xlab = "FEs", ylab = "Performance")
            points(train.xData, train.yData, pch = 20, col = "blue" )
            lines(xData, weight_yData, col = "green")
            title(main = tmp.dat[1, "instance_file"] %>% as.character)
        }
        
        
        
        #save each train and test set to finall error res table : all.error.res
       if(is.null(all.error.res))
           all.error.res <- error.res[, 1]
        all.error.res <- all.error.res %>% cbind(error.res[, 2])
        all.error.res.colnames <- all.error.res.colnames %>% cbind(paste(maxTrain[train], "_", maxTest[test], sep = ""))
        
        dev.off()
    }
    
}

colnames(all.error.res) <- all.error.res.colnames
write.csv(all.error.res, all.error.res.csvname, row.names = FALSE)
    

