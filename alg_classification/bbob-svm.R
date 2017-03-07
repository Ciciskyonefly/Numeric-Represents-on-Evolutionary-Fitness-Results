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


#, expLinearModelpositive, logisticModelpositive, decayModelnegative)
loc.path = "./modelresults/LM.bbob.pre/bbob-singleRun-log-y/BL_10_LGMP.csv"
all.path <- loc.path %>% list.files()
library(dplyr)
dat <- LoadBbob(loc.path)

dat <- dat[-which(dat$a == 0)]
# getSub <- function(dat, alg, func){
#     sub.alg <-  which(dat$alg == alg)
#     sub.func <- which(dat$func == func)
#     return(intersect(sub.alg, sub.func))
# }
# 
# CMASE.f10.sub <- getSub(dat, "hill", "f11")
# CMASE.f11.sub <- getSub(dat, "CMAES", "f11")


classification.dat <- select(dat[ which(dat$func == "f1"), ], a, b, c, d, alg)
classification.dat$a <- exp(classification.dat$a)
classification.dat <- classification.dat[- which(classification.dat$alg == "IPOP"), ]
classification.dat <- classification.dat[- which(classification.dat$alg == "IPOP-tany"), ]

#classification.dat <- select(dat[c(CMASE.f10.sub, CMASE.f11.sub), ], a, b, c, d, func)
classification.dat$alg <- classification.dat$alg %>% droplevels() %>% as.numeric() %>% factor()

traindata <- classification.dat
svm.residuals = 0
for(j in 1:2){
    
    train.index <- createDataPartition(traindata[, 1], p = 0.8, list = FALSE)
    mytrain <- traindata[train.index, ]
    mytest <- traindata[-train.index, ]
    
    
    linear.grid <- expand.grid(
        C = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.4)
    )
    
    ctrl <- trainControl(method = "CV", 
                         number = 10,
                         repeats = 1
    )
    
    
    
    svmLinear <- caret::train(alg~.,
                       data = mytrain,
                       metric = "Accuracy",
                       method = "svmLinear",
                       #          preProc = c("center","scale"),
                       num_class =  levels( classification.dat$alg) %>% length(),
                       trControl = ctrl,
                       tuneGrid = linear.grid,
                       verbose=TRUE
    )
    linear.pre <- predict(svmLinear, mytest[, -ncol(mytest)])
    conf.matrix <- confusionMatrix(mytest$alg, linear.pre ) 
    print(conf.matrix)
  #  print(svm.residuals)
    
    svm.residuals = svm.residuals + sum(diag(conf.matrix))/sum(conf.matrix)
    
}

svm.residuals = svm.residuals/2

print(svm.residuals)
#0.9958333