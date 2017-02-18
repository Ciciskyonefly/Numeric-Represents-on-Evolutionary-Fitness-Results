rm(list = ls())
source("./base-funcs/maxsattsp_load_and_order_func.R")
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

alldata <- LoadBbob()
traindata <- dplyr::select(alldata, a, b, c, d, residuals,  func.lab)
traindata$func.lab <- traindata$func.lab %>% as.numeric()-1 
traindata$func.lab  <- factor(traindata$func.lab)
train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
train <- traindata[train.index, ]
test <- traindata[-train.index, ]
train.mx <- sparse.model.matrix(func.lab~ -1 + ., train)
test.mx <- sparse.model.matrix(func.lab~ -1 + ., test)

linear.grid <- expand.grid(
    C = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.4)
)
ctrl <- trainControl(method = "cv", 
                     number = 10
)



svmLinear <- train(func.lab~.,
                   data = train,
                   metric = "Accuracy",
                   method = "svmLinear",
                   preProc = c("center","scale"),
                   num_class = 5,
                   trControl = ctrl,
                   tuneGrid = linear.grid,
                   verbose=TRUE
)
print(svmLinear$bestTune)
linear.pre <- predict(svmLinear, test[, -6])
conf.matrix <- confusionMatrix(test$func.lab, linear.pre ) 
sum(diag(conf.matrix$table))/sum(conf.matrix$table)
