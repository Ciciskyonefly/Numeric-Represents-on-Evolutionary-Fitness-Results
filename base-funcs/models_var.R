


#include realtive plot function file
#up-to-the minute 06_24
#TODO res$residual
#use x,y dimension to compute errpr
#rm(list = ls())
source("./base-funcs/plotCurve_linear_single.R")
source("./base-funcs/plotCurve_logistic_single.R")
source("./base-funcs/plotCurve_decay_single.R")
source("./base-funcs/plotCurve_gompert_single.R")
#source("../R_Code/xyRMSE.R")
#include nls
library("BB")
#include RMSE
#library(qpcR)
library("minpack.lm")
library(Formula)
#we should get the data s first
# First we define the models




## example model 1 -----function
logisticModelFunction <-  function(modelParams, xData) {
        modelParams[1] + modelParams[2]/(1+exp(modelParams[3]*log(xData)+modelParams[4])) 
}


#logisticModelFunction <- function(modelParams,xData){
#        # Three parameters function
#}


gompertzModelFunction <-  function(modelParams, xData) {
        modelParams[1]+ modelParams[2]*exp(modelParams[3]*exp(xData*modelParams[4])) 
}

#use the logx dimension formula
decayModelFunction <- function(modelParams,xData){
        modelParams[1]+(modelParams[2]*exp(modelParams[3]*xData^modelParams[4]))
}

expLinearModelFunction <- function(modelParams,xData){
        
        modelParams[1]+modelParams[2]*exp(modelParams[3]*log(modelParams[4] + xData))
}





logisticModelpositive <- NULL
logisticModelpositive$name <- 'logisticModelpositive'
logisticModelpositive$modelFunction <- logisticModelFunction
logisticModelpositive$nParameters <- 4
logisticModelpositive$formula <- y ~ a + b/(1+exp(c*log(x) + d))
logisticModelpositive$plotFunction <- plotCurve_exp_3_single
logisticModelpositive$logX <-  FALSE
logisticModelpositive$initFunc <- function(xData,yData){
        #c(max(yData)-0.3*max(yData)*runif(1),3*rnorm(1),-6*runif(1))
        c(runif(1),max(yData)-0.3*max(yData)*runif(1),runif(1),runif(1))
}
logisticModelpositive$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
logisticModelpositive$flag <- 1
logisticModelpositive$iter <- 100
logisticModelpositive$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"/(1+exp(",as.character(ppar[3]),"*log(FEs)+",as.character(ppar[4]),"))" ,sep = "")
}




logisticModelnegative <- NULL
logisticModelnegative$name <- 'logisticModelnegative'
logisticModelnegative$modelFunction <- logisticModelFunction
logisticModelnegative$nParameters <- 4
logisticModelnegative$formula <- y ~ a + c/(1+exp(c*log(x) + d))
logisticModelnegative$plotFunction <- plotCurve_exp_3_single
logisticModelnegative$logX <-  FALSE
logisticModelnegative$initFunc <- function(xData,yData){
        #c(max(yData)-0.3*max(yData)*runif(1),3*rnorm(1),-6*runif(1))
        c(runif(1),-max(yData)-0.3*max(yData)*runif(1),2*runif(1),-runif(1))
}
logisticModelnegative$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
logisticModelnegative$flag <- -1
logisticModelnegative$iter <- 100
logisticModelnegative$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"/(1+exp(",as.character(ppar[3]),"*log(FEs)+",as.character(ppar[4]),"))" ,sep = "")
}



gompertzModelnegative <- NULL
gompertzModelnegative$name <- 'gompertzModelnegative'
gompertzModelnegative$modelFunction <- gompertzModelFunction
gompertzModelnegative$nParameters <- 4
gompertzModelnegative$formula <- y ~ a+b*exp(c*exp(x*d))
gompertzModelnegative$plotFunction <- plotCurve_gompert_single
gompertzModelnegative$logX <-  FALSE
gompertzModelnegative$flag <- -1
gompertzModelnegative$initFunc<- function(xData,yData) {
        c(a = runif(1),b = -max(yData)-0.2*max(yData)+runif(1),c = -runif(1),d = 0.5)
}
gompertzModelnegative$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
gompertzModelnegative$iter <- 100
gompertzModelnegative$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}




gompertzModelpositive <- NULL
gompertzModelpositive$name <- 'gompertzModelpositive'
gompertzModelpositive$modelFunction <- gompertzModelFunction
gompertzModelpositive$nParameters <- 4
gompertzModelpositive$formula <- y ~ a+b*exp(c*exp(x*d))
gompertzModelpositive$plotFunction <- plotCurve_gompert_single
gompertzModelpositive$logX <-  FALSE
gompertzModelpositive$initFunc<- function(xData,yData) {
        c(a = 2+rnorm(1),b = max(yData)-0.2*max(yData)+runif(1),c = -runif(1),d = runif(1))
        
}
gompertzModelpositive$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
gompertzModelpositive$iter <- 100
gompertzModelpositive$flag <- 1
gompertzModelpositive$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}



decayModelpositive <- NULL
decayModelpositive$name <- 'decayModelpositive'
decayModelpositive$symbol <- 1
decayModelpositive$modelFunction <- decayModelFunction
decayModelpositive$nParameters <- 4
decayModelpositive$formula <- y ~ a +(b*exp(c*x^d))
decayModelpositive$plotFunction <- plotCurve_tweise_single
decayModelpositive$flag <-  1
decayModelpositive$initFunc <- function(xData,yData) {
        c(runif(1),max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-abs(rnorm(1,mean = 0,sd = 0.1)),5*runif(1))
}
decayModelpositive$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
decayModelpositive$iter <- 100
decayModelpositive$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
}



decayModelnegative <- NULL
decayModelnegative$name <- 'decayModelnegative'
decayModelnegative$symbol <- -1
decayModelnegative$modelFunction <- decayModelFunction
decayModelnegative$nParameters <- 4
decayModelnegative$formula <- y ~ a +(b*exp(c*x^d))
decayModelnegative$plotFunction <- plotCurve_tweise_single
decayModelnegative$flag <-  -1
decayModelnegative$initFunc <- function(xData,yData) {
        
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-max(yData),-abs(rnorm(1,mean = 0,sd = 0.1)),-abs(5*runif(1)))
        
}
decayModelnegative$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
decayModelnegative$iter <- 100
decayModelnegative$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}






# model 2.1 LogyLogx linear model
expLinearModelpositive <- NULL
expLinearModelpositive$name <- 'expLinearModelpositive'
expLinearModelpositive$modelFunction <- expLinearModelFunction
expLinearModelpositive$formula <- y~a+b*exp(c*log(x+d))
expLinearModelpositive$nParameters <- 4
expLinearModelpositive$plotFunction <- plotCurve_line_logy_logx3_single
expLinearModelpositive$initFunc <- function(xData,yData) {
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),abs(max(yData)*rnorm(1)),-abs(rnorm(1,mean = 0,sd = 0.1)),5*runif(1))
}
expLinearModelpositive$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
expLinearModelpositive$iter <- 100
expLinearModelpositive$flag <- 1
expLinearModelpositive$printFormula <- function(ppar){
        #  paste("exp(",as.character(ppar[1]),"+",as.character(ppar[2]),"*log(FEs))" ,sep = "")
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*log(FEs+",as.character(ppar[4]),"))" ,sep = "")
}







expLinearModelnegative <- NULL
expLinearModelnegative$name <- 'expLinearModelnegative'
expLinearModelnegative$modelFunction <- expLinearModelFunction
expLinearModelnegative$formula <- y~a+b*exp(c*log(x+d))
expLinearModelnegative$nParameters <- 4
expLinearModelnegative$flag <- -1
expLinearModelnegative$plotFunction <- plotCurve_line_logy_logx3_single
expLinearModelnegative$initFunc <- function(xData,yData) {
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-max(yData),runif(1),rnorm(1))
}
expLinearModelnegative$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
expLinearModelnegative$iter <- 100
expLinearModelnegative$printFormula <- function(ppar){
        #  paste("exp(",as.character(ppar[1]),"+",as.character(ppar[2]),"*log(FEs))" ,sep = "")
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*log(FEs+",as.character(ppar[4]),"))" ,sep = "")
}
