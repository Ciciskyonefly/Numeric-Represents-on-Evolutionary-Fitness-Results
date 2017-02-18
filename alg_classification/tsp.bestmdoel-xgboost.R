

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

start.time <- Sys.time()

loc.path = "./modelresults/LM.tsp.pre/100percentleft/"
all.path <- loc.path %>% list.files()


classResultsTabular <- NULL

set.seed(59)

temp <- NULL

modelresults <- NULL
xgb.residuals <- 0
for( j in 1: 10){
        #change for other data file classification in TSP
        model.pattern <- paste("10_gompertzModelpositive_model.csv", sep = "")
        file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
        alldata <- LoadTsp(file.path)
        binary.algorithms <- alldata$algorithms %>% as.character()
        binary.algorithms[grepl("b", binary.algorithms)] <- 0
        binary.algorithms[grepl("f", binary.algorithms)] <- 1
        alldata$algorithms <- binary.algorithms 
        traindata <- dplyr::select(alldata, a, b, c, d, residuals,  algorithms)
        traindata$algorithms <- traindata$algorithms %>% as.numeric()
        traindata$algorithms <- factor(traindata$algorithms)
        
        
        train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
        train <- traindata[train.index, ]
        test <- traindata[-train.index, ]
        train.mx <- sparse.model.matrix(algorithms~ -1 + ., train)
        test.mx <- sparse.model.matrix(algorithms~ -1 + ., test)
        
        
        
        ctrl <- trainControl(method = "cv",
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
                   #               num_class = 2,
                                  trControl = ctrl,
                                  print.every.n = 10,
                                  tuneGrid = xgbGrid,
                                  verbose=TRUE
        )
        
        xgb.pre <- predict(xgb_model, test[, -6])
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
        
        
        xgb.residuals <- xgb.residuals + sum(diag(yliu))/sum(sum(yliu))
        
        
} 

xgb.residuals <- xgb.residuals/10
xgb.residuals %>% print()
end.time <- Sys.time()

print(end.time - start.time)

#0.9666667
#0.975

# classResultsTabular = rbind(classResultsTabular, cbind(model = list.model[[mod]]$name, residuals))
# 
# write.csv(classResultsTabular, file = "./alg_classification/xgboost.csv", row.names = FALSE)
# 
# writeTabular(classResultsTabular,
#              print.col.names = TRUE, print.row.names = FALSE, digits = 4,
#              wrap.as.table = TRUE, table.position = "hbtp",
#              caption.position = "t", centering = TRUE
# )


