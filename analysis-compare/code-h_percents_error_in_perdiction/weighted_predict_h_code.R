remove(list = ls())
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
library(dplyr)
predict.path <- "./modelresults/LM.tsp.pre/singleRun/maxTrain-maxTest/"
rawdata.path <- "./rawdata/tsp-singleRun/"
dat.list <- predict.path %>% list.files()
maxTrain <- c(10, 50, 100)
maxTest <- c(100, 1000, 10000)

train <- 1
test <- 1

#TODO
pdfname <- "..."
pdf(pdfname)
par(mfrow= c(2,2))

for(train in 1:length(maxTrain)){
    
    for(test in 1:length(maxTest)){
        
        pattern <- paste(maxTrain[train], "_", maxTest[test],"_.+.csv", sep = "")
        Train.Test.dat.list <- paste(predict.path, dat.list[grep(pattern, dat.list)], sep = "") 
        
        #all dat in every single maxTrain and maxTest couple
        all.dat <- NULL
        for(dat in 1:length(Train.Test.dat.list)){
            all.dat <- all.dat %>% rbind(LoadTsp(Train.Test.dat.list[dat]))
        }
       dat.split <- all.dat %>% split(all.dat$instance_file, all.dat$model, drop = TRUE) 
       split <- 1
       for(split in 1:length(split)){
           
           temp.dat <- dat.split[[split]][order(dat.split[[split]]$residuals), ]
           
           rawdata.name <- gsub("/", "_", temp.dat[1, "instance_file"] %>% as.character)
           rawdata <- paste(rawdata.path, rawdata.name, ".csv", 
                            sep = "") %>% read.csv()
           
           xData <- rawdata$x[which(rawdata$x < maxTest[test])]
           yData <- rawdata$y[which(rawdata$x < maxTest[test])]
           train.xData <- rawdata$x[which(rawdata$x < maxTrain[train])]
           train.yData <- rawdata$y[which(rawdata$x < maxTrain[train])]
           
           weight_yData <- NULL
           for(j in 1:nrow(temp.dat)){
               
               par <- temp.dat[j, c("a", "b", "c", "d")] %>% as.vector %>% as.numeric
               if(is.null(weight_yData)){
                   weight_yData <- eval(parse(text = temp.dat$model[j] %>% as.character))$modelFunction(par, xData)
               } else {
                   weight_yData <- weight_yData + (nrow(temp.dat)-j+1) *eval(parse(text = temp.dat$model[j] %>% as.character))$modelFunction(par, xData)
               }
           }
           
           weight_yData <- weight_yData/sum(1:nrow(temp.dat))
           
           plot(xData, yData, pch = 20, log = "x", xlab = "FEs", ylab = "Performance")
           points(train.xData, train.yData, pch = 20, col = "blue" )
           lines(xData, weight_yData, col = "green")
           title(main = temp.dat[1, "instance_file"] %>% as.character)
       }
       
       test <- aggregate(residuals ~ instance_file, data = all.dat, FUN = sort)
        all.dat %>% head()  
    }
}

dev.off()