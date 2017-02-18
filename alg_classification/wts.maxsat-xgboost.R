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

#set.seed(3456)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")

iter = 20
hiddensize = 6
maxiter = 50


loc.path = "./modelresults/NN.maxsat.pre/100percentleft/"
pattern <- paste( "wts_residuals" ,iter, "iter_", hiddensize, "hiddensize_", maxiter, "maxiter_",  sep = "")
all.path <- loc.path %>% list.files()
classResultsTabular <- NULL
set.seed(59)
temp <- NULL
#for(mod in 1:length(list.model)){
accuracy <- 0
modelresults <- NULL

k = 1

for( j in 1: k){
  file.path <- paste(loc.path, all.path[grep(pattern, all.path)], sep = "")
  alldata <- read.csv(file.path, stringsAsFactors = FALSE)
  colnames(alldata)[1] <- "instance_file"
  data.matrix <- alldata
  uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms.maxsat()
  uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2] 
  instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
  data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
  data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)
  
  alldata <- data.matrix
  
  
  alldata <- alldata[-which(alldata$instances == "experiment.xml"), ]
  traindata <- dplyr::select(alldata, contains("X"),  algorithms)
  traindata$algorithms <- traindata$algorithms %>% as.numeric()-1
  traindata$algorithms <- factor(traindata$algorithms)
  
  
  train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
  train <- traindata[train.index, ]
  test <- traindata[-train.index, ]
  train.mx <- sparse.model.matrix(algorithms~ -1 + ., train)
  test.mx <- sparse.model.matrix(algorithms~ -1 + ., test)
  
  
  
  ctrl <- trainControl(method = "LOOCV",
                       number = 10
  )
  xgbGrid <- expand.grid(
    eta = c(0.05, 0.1, 0.2),
    max_depth =c( 9, 12),
    nrounds = c(100),
    gamma = c(0),
    colsample_bytree = c(0.9, 1.0),
    subsample = c(0.4,  0.7,  0.9),
    min_child_weight = 1
  )
  
  
  
 
   xgb_model <- caret::train(algorithms~.,
                            data = train,
                            method = "xgbTree",
                            metric = "Accuracy",
                            num_class = 6,
                            trControl = ctrl,
                            print.every.n = 10,
                            tuneGrid = xgbGrid,
                            verbose=TRUE
  )
  
  xgb.pre <- predict(xgb_model, test[, -ncol(test)])
  yliu <- confusionMatrix(test$algorithms, xgb.pre)
  
  
  #xgboost包
  # dtrain <- xgb.DMatrix(train.mx, label=train$algorithms )
  # dtest <- xgb.DMatrix(test.mx, label=test$algorithms)
  # 
  # model <- xgb.train(data = dtrain,
  #                    watchlist = list(test = dtrain, train = dtest),
  #                    params = list(
  #                            objective = 'multi:softmax',
  #                            eta = 0.2,
  #                            max_depth = 20,
  #                            subsample = 0.6,
  #                            num_class = 6
  #                    ),
  #                    nrounds = 150,
  #                    print.every.n = 10,
  #                    maximize = FALSE
  # )
  # xgb.pre <- predict(xgb_model, test[, -6])
  # yliu <- confusionMatrix(test$algorithms, xgb.pre)
  
  accuracy <- accuracy + sum(diag(yliu$table))/sum(yliu$table)
  
} 

residuals <- accuracy/k

print(residuals)



































