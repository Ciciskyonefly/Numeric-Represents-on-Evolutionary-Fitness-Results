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


list.model <- list(logisticModelpositive, expLinearModelpositive, decayModelpositive, decayModelnegative)
#list.model <- list(expLinearModelpositive)
#, expLinearModelpositive, logisticModelpositive, decayModelnegative)
loc.path = "./modelresults/LM.maxsat.pre/100percentleft/"
all.path <- loc.path %>% list.files()


classResultsTabular <- NULL
set.seed(60)
for(mod in 1:length(list.model)){
        
        ANN.residuals = 0
        
        for(j in 1:10){
                model.pattern <- paste("10_", list.model[[mod]]$name, ".+.csv", sep = "")
                file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
                alldata <- LoadMaxsat(file.path)
                print(file.path)
                alldata <- alldata[-which(alldata$instances == "experiment.xml"), ]
                traindata <- dplyr::select(alldata, a, b, c, d, residuals,  algorithms)
                traindata$algorithms <- traindata$algorithms %>% as.numeric()-1
                traindata$algorithms <- factor(traindata$algorithms)
                
               
                train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
                mytrain <- traindata[train.index, ]
                mytest <- traindata[-train.index, ]
                
                
                nnetGrid <- expand.grid(
                        size = c(4, 6, 8, 10, 12, 14),
                        decay = c(0.01, 0.02, 0.03)
                        
                )
                
                nnetContrl <- trainControl(method = "cv", number = 10)
                
                nnet.model <- caret::train(algorithms ~ . ,
                                           data = mytrain,
                                           method = "nnet",
                                           metric = "Accuracy",
                                           num_class = 6,
                                           tuneGrid = nnetGrid,
                                           trainControl = nnetContrl,
                                           trace = FALSE,
                                           nrounds = 100              
                )
                print(nnet.model$bestTune)
                ann.pre <- predict(nnet.model, mytest[, -6])
                QIQI <- confusionMatrix(mytest$algorithms, ann.pre)
                
                table(ann.pre, mytest$algorithms)
                ANN.residuals <- ANN.residuals + (sum(diag(QIQI))/sum(sum(QIQI)))
                print(sum(diag(QIQI))/sum(sum(QIQI)))
        }
        
        ANN.residuals = ANN.residuals/10
        
        print(ANN.residuals)
        
        classResultsTabular = rbind(classResultsTabular, cbind(model = list.model[[mod]]$name, ANN.residuals))
        
}

write.csv(classResultsTabular, file = "./alg_classification/ANNres.csv", row.names = FALSE)
# 

classResultsTabular %>% print()

# [1,] "logisticModelpositive"  "0.853333333333333"
# [2,] "expLinearModelpositive" "0.6275"
# [3,] "decayModelpositive"     "0.7475"           
# [4,] "decayModelnegative"     "0.8225" 

