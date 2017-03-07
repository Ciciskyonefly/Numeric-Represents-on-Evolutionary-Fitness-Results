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
loc.path = "./modelresults/LM.bbob.pre/bbob-singleRun-log-y/BL_10_LGMP.csv"
all.path <- loc.path %>% list.files()
library(dplyr)
dat <- LoadBbob(loc.path)

# getSub <- function(dat, alg, func){
#     sub.alg <-  which(dat$alg == alg)
#     sub.func <- which(dat$func == func)
#     return(intersect(sub.alg, sub.func))
# }
# 
# CMASE.f10.sub <- getSub(dat, "hill", "f11")
# CMASE.f11.sub <- getSub(dat, "CMAES", "f11")


 classification.dat <- select(dat[ which(dat$func == "f12"), ], a, b, c, d, alg)
 classification.dat <- classification.dat[- which(classification.dat$alg == "IPOP"), ]
 classification.dat <- classification.dat[- which(classification.dat$alg == "IPOP-tany"), ]
 
#classification.dat <- select(dat[c(CMASE.f10.sub, CMASE.f11.sub), ], a, b, c, d, func)
classification.dat$alg <- classification.dat$alg %>% droplevels() %>% as.numeric() %>% factor()

traindata <- classification.dat
ANN.residuals = 0
for(j in 1:2){
    
    train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
    mytrain <- traindata[train.index, ]
    mytest <- traindata[-train.index, ]
    
    nnetGrid <- expand.grid(
        size = c(6, 4, 8, 10),
        decay = c(0.01, 0.02)
        
    )
    
    nnetContrl <- trainControl(method = "cv", number = 10)
    
    nnet.model <- caret::train(alg ~ . ,
                               data = mytrain,
                               method = "nnet",
                               metric = "Accuracy",
                               num_class =  levels( classification.dat$alg) %>% length(),
                               print.every.n = 100,
                               tuneGrid = nnetGrid,
                               trainControl = nnetContrl,
                               trace = FALSE,
                               verbose = 0,
                               nrounds = 100               
    )
    
    ann.pre <- predict(nnet.model, mytest[, -5])
    QIQI <- confusionMatrix(mytest$alg, ann.pre)
    
    table(ann.pre, mytest$alg)
    ANN.residuals <- ANN.residuals + (sum(diag(QIQI))/sum(sum(QIQI)))
    
}

ANN.residuals = ANN.residuals/10

print(ANN.residuals)
#0.9958333