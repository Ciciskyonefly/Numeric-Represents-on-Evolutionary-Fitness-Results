
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
library(logicFS)
#set.seed(3456)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")

#list.model <- list(logisticModelpositive, expLinearModelpositive, decayModelpositive, decayModelnegative)

loc.path = "./modelresults/LM.tsp.pre/100percentleft/"
all.path <- loc.path %>% list.files()


classResultsTabular <- NULL

set.seed(59)

modelresults <- NULL
LR.residuls <- 0
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
        
        
        train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
        train <- traindata[train.index, ]
        test <- traindata[-train.index, ]
        train.mx <- sparse.model.matrix(algorithms~ -1 + ., train)
        test.mx <- sparse.model.matrix(algorithms~ -1 + ., test)
        
        
        
        ctrl <- trainControl(method = "cv",
                             number = 10
        )
        
        # glm_model <- glm(algorithms~.,
        #                  data = train,
        #                  family="binomial"
        # )
        # 
        
        glm_model <- caret::train(algorithms~.,
                                  data = train,
                                  method="lm",
                                  trControl = ctrl,
                                  family = "binomial",
                                  metric = "R-squared",
                                  verbose=TRUE
        )
        
        glm.pre <- predict(glm_model, test[, -6], type = "raw")
        glm.pre[glm.pre < 0.5] <- 0
        glm.pre[glm.pre > 0.5] <- 1
        yliu <- confusionMatrix(test$algorithms, glm.pre)
        
        
        LR.residuls <- LR.residuls + sum(diag(yliu))/sum(yliu)
        
        
        
} 



LR.residuls <- LR.residuls/10
#1


print(LR.residuls)


























# names <- names(train)[-7]
# importance_matrix <- xgb.importance(names, model = model)
# xgb.plot.importance(importance_matrix)



# ins.sparse <- sparse.model.matrix(~ -1 + instances, drop.unused.levels = TRUE, alldata)
# alg.sparse <- sparse.model.matrix(~ -1 + algorithms, drop.unused.levels = TRUE, alldata)
# labels <- cbind(ins.sparse, alg.sparse) %>% as.matrix() %>% data.frame()
# traindata <- cbind(traindata[, c(1:5)], labels)
# ncol(traindata)
# 
# traindata %>% head()
# train = xgb.DMatrix(data = traindata[, c(1:15)], labels = alldata$algorithms)
# train.list <- list(data = traindata[, c(1:15)], labels = traindata[, c(16:21)])
# train = xgb.DMatrix(data = train.list$data, labels = train.list$labels)
# num(traindata$algorithms)



