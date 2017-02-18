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
library(xgboost)
library(LiblineaR)
set.seed(3456)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")


#list.model <- list(logisticModelpositive, expLinearModelpositive, decayModelpositive, decayModelnegative)
list.model <- list(decayModelpositive, logisticModelpositive, expLinearModelpositive, decayModelnegative)
loc.path = "./modelresults/LM.maxsat.pre/100percentleft/"
all.path <- loc.path %>% list.files()

classResultsTabular <- NULL

set.seed(59)

for(mod in 1:length(list.model)){
        
        svm.residuals = 0
        
        for(j in 1:10){
                
                model.pattern <- paste("_", list.model[[mod]]$name, ".+.csv", sep = "")
                file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
                alldata <- LoadMaxsat(file.path)
                
                
                alldata <- alldata[-which(alldata$instances == "experiment.xml"), ]
                traindata <- dplyr::select(alldata, a, b, c, d, residuals,  algorithms)
                traindata$algorithms <- traindata$algorithms %>% as.numeric()-1 
                traindata$algorithms  <- factor(traindata$algorithms)
                train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
                train <- traindata[train.index, ]
                test <- traindata[-train.index, ]
                train.mx <- sparse.model.matrix(algorithms~ -1 + ., train)
                test.mx <- sparse.model.matrix(algorithms~ -1 + ., test)
                
                linear.grid <- expand.grid(
                        C = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.4)
                )
                ctrl <- trainControl(method = "cv", 
                                     number = 10
                )
                
                
                
                svmLinear <- train(algorithms~.,
                                   data = train,
                                   metric = "Accuracy",
                                   method = "svmLinear",
                                   #          preProc = c("center","scale"),
                                   num_class = 6,
                                   trControl = ctrl,
                                   tuneGrid = linear.grid,
                                   verbose=TRUE
                )
                print(svmLinear$bestTune)
                linear.pre <- predict(svmLinear, test[, -6])
                conf.matrix <- confusionMatrix(test$algorithms, linear.pre ) 
                
                svm.residuals = svm.residuals + sum(diag(conf.matrix$table))/sum(conf.matrix$table)
        }
        
        svm.residuals = svm.residuals/10
        
        print(svm.residuals)
         
        classResultsTabular = rbind(classResultsTabular, cbind(model = list.model[[mod]]$name, svm.residuals))
        
}

write.csv(classResultsTabular, file = "./alg_classification/maxsat_LM_svm.csv", row.names = FALSE)
 classResultsTabular %>% print()
# [1,] "logisticModelpositive"  "0.790833333333333"
# [2,] "expLinearModelpositive" "0.805833333333333"
# [3,] "decayModelpositive"     "0.769166666666667"           
# [4,] "decayModelnegative"     "0.819166666666667"

 
 
# 
# radial.grid <- expand.grid(
#         C = c(1.1, 1.2, 1.25, 1.3, 1.4),
#         sigma = c(0, 0.1, 0.2, 0.3)
#         )
# svmRadial <- train(algorithms~.,
#                    data = train,
#                    metric = "Accuracy",
#                    method = "svmRadial",
#                    preProc = c("center","scale"),
#                    num_class = 6, 
#                    trControl = ctrl,
#                    tuneGrid = radial.grid,
#                    verbose=TRUE
# )
# radial.pre <- predict(svmRadial, test[, -6])
# QIQI <- confusionMatrix(test$algorithms, radial.pre)
# QIQI %>% print()
# svmCompare <- list(svm1 = svmLinear, svm2 = svmRadial) %>% resamples()
# bwplot(svmCompare, metric = "Accuracy", ylab =c("linear kernel", "radial kernel"))

