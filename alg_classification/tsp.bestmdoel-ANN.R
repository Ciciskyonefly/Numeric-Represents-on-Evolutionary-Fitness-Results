rm(list = ls())
library(caret)
library(dplyr)
library(nnet)
library(ggplot2)
library(stringr)
library(data.table)
library(varhandle)
library(scmamp)
library(Matrix)
library(ggplot2)
library(RSNNS)
library(nnet)

source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")


#, expLinearModelpositive, logisticModelpositive, decayModelnegative)
loc.path = "./modelresults/LM.tsp.pre/singleRun/100percentleft/"
all.path <- loc.path %>% list.files()


classResultsTabular <- NULL
set.seed(59)

ANN.residuals = 0

for(j in 1:10){
        #change for other data file classification in TSP
        model.pattern <- paste("10_gompertzModelpositive_model.csv", sep = "")
        file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
        alldata <- LoadTsp(file.path)
        print(file.path)
        
        
        binary.algorithms <- alldata$algorithms %>% as.character()
        binary.algorithms[grepl("b", binary.algorithms)] <- 0
        binary.algorithms[grepl("f", binary.algorithms)] <- 1
        alldata$binary.algorithms <- binary.algorithms
        traindata <- dplyr::select(alldata, a, b, c, d, residuals,  binary.algorithms)
        traindata$binary.algorithms <- traindata$binary.algorithms %>% as.numeric()
        traindata$binary.algorithms <- factor(traindata$binary.algorithms)
        
        # traindata <- dplyr::select(alldata, a, b, c, d, residuals,  algorithms)
        # traindata$algorithms <- traindata$algorithms %>% as.numeric()-1
        # traindata$algorithms <- factor(traindata$algorithms)
        
        train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
        mytrain <- traindata[train.index, ]
        mytest <- traindata[-train.index, ]
        
        
        nnetGrid <- expand.grid(
                size = c(6, 4, 8, 10),
                decay = c(0.01, 0.02)
                
        )
        
        nnetContrl <- trainControl(method = "cv", number = 10)
        
        nnet.model <- caret::train(binary.algorithms ~ . ,
                                   data = mytrain,
                                   method = "nnet",
                                   metric = "Accuracy",
                                   num_class = 2,
                                   print.every.n = 100,
                                   tuneGrid = nnetGrid,
                                   trainControl = nnetContrl,
                                   trace = FALSE,
                                   verbose = 0,
                                   nrounds = 100               
        )
        
        ann.pre <- predict(nnet.model, mytest[, -6])
        QIQI <- confusionMatrix(mytest$binary.algorithms, ann.pre)
        
        table(ann.pre, mytest$binary.algorithms)
        ANN.residuals <- ANN.residuals + (sum(diag(QIQI))/sum(sum(QIQI)))
        
}

ANN.residuals = ANN.residuals/10

print(ANN.residuals)
#0.9958333

