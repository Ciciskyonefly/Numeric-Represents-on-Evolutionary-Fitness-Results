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

list.model <- list(logisticModelpositive, expLinearModelpositive, logisticModelpositive, decayModelnegative)
#, expLinearModelpositive, logisticModelpositive, decayModelnegative)
loc.path = "./modelresults/LM.maxsat.pre/100percentleft/"
all.path <- loc.path %>% list.files()

alldata = NULL
for(mod in 1:length(list.model)){
model.pattern <- paste("10_", list.model[[mod]]$name, ".+.csv", sep = "")
file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
temp.data <- LoadMaxsat(file.path)

alldata = rbind(alldata, temp.data)
print(file.path)

}


alldata <- alldata[-which(alldata$instances == "experiment.xml"), ]

write.csv(alldata, file = "maxsatalltraindata.csv", row.names = FALSE)
traindata <- dplyr::select(alldata, a, b, c, d, residuals,  algorithms)
traindata$algorithms <- traindata$algorithms %>% as.numeric()-1
traindata$algorithms <- factor(traindata$algorithms)

set.seed(59)
train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
train <- traindata[train.index, ]
test <- traindata[-train.index, ]

linear.grid <- expand.grid(
      #  C = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.4)
        C = c(1.1)
)
ctrl <- trainControl(method = "LOOCV", 
                     number = 10,
                     repeats = 1
)



svmLinear <- caret::train(algorithms~.,
                   data = train,
                   metric = "Accuracy",
                   method = "svmLinear",
                   #          preProc = c("center","scale"),
                   num_class = 6,
                   trControl = ctrl,
                   tuneGrid = linear.grid,
                   verbose=TRUE
)

linear.pre <- predict(svmLinear, test[, -6])
QIQI <- confusionMatrix(test$algorithms, linear.pre ) 


# ann.pre <- predict(nnet.model, test[, -6])
# QIQI <- confusionMatrix(test$algorithms, ann.pre)

table(ann.pre, test$algorithms)

temp.res = cbind(model.names = list.model[[mod]]$name, nnet.model$bestTune,Accuracy = (sum(diag(QIQI))/sum(sum(QIQI))) )
res = NULL
res = rbind(res, temp.res)