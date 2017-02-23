


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





LGMP <- NULL
LGMP$name <- 'LGMP'
LGMP$modelFunction <- logisticModelFunction
LGMP$nParameters <- 4
LGMP$formula <- y ~ a + b/(1+exp(c*log(x) + d))
LGMP$plotFunction <- plotCurve_exp_3_single
LGMP$logX <-  FALSE
LGMP$initFunc <- function(xData,yData){
        #c(max(yData)-0.3*max(yData)*runif(1),3*rnorm(1),-6*runif(1))
        c(runif(1),max(yData)-0.3*max(yData)*runif(1),runif(1),runif(1))
}
LGMP$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
LGMP$flag <- 1
LGMP$iter <- 100
LGMP$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"/(1+exp(",as.character(ppar[3]),"*log(FEs)+",as.character(ppar[4]),"))" ,sep = "")
}




LGMN <- NULL
LGMN$name <- 'LGMN'
LGMN$modelFunction <- logisticModelFunction
LGMN$nParameters <- 4
LGMN$formula <- y ~ a + c/(1+exp(c*log(x) + d))
LGMN$plotFunction <- plotCurve_exp_3_single
LGMN$logX <-  FALSE
LGMN$initFunc <- function(xData,yData){
        #c(max(yData)-0.3*max(yData)*runif(1),3*rnorm(1),-6*runif(1))
        c(runif(1),-max(yData)-0.3*max(yData)*runif(1),2*runif(1),-runif(1))
}
LGMN$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
LGMN$flag <- -1
LGMN$iter <- 100
LGMN$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"/(1+exp(",as.character(ppar[3]),"*log(FEs)+",as.character(ppar[4]),"))" ,sep = "")
}



GPMN <- NULL
GPMN$name <- 'GPMN'
GPMN$modelFunction <- gompertzModelFunction
GPMN$nParameters <- 4
GPMN$formula <- y ~ a+b*exp(c*exp(x*d))
GPMN$plotFunction <- plotCurve_gompert_single
GPMN$logX <-  FALSE
GPMN$flag <- -1
GPMN$initFunc<- function(xData,yData) {
        c(a = runif(1),b = -max(yData)-0.2*max(yData)+runif(1),c = -runif(1),d = 0.5)
}
GPMN$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
GPMN$iter <- 100
GPMN$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}




GPMP <- NULL
GPMP$name <- 'GPMP'
GPMP$modelFunction <- gompertzModelFunction
GPMP$nParameters <- 4
GPMP$formula <- y ~ a+b*exp(c*exp(x*d))
GPMP$plotFunction <- plotCurve_gompert_single
GPMP$logX <-  FALSE
GPMP$initFunc<- function(xData,yData) {
        c(a = 2+rnorm(1),b = max(yData)-0.2*max(yData)+runif(1),c = -runif(1),d = runif(1))
        
}
GPMP$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
GPMP$iter <- 100
GPMP$flag <- 1
GPMP$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}



DCMP <- NULL
DCMP$name <- 'DCMP'
DCMP$symbol <- 1
DCMP$modelFunction <- decayModelFunction
DCMP$nParameters <- 4
DCMP$formula <- y ~ a +(b*exp(c*x^d))
DCMP$plotFunction <- plotCurve_tweise_single
DCMP$flag <-  1
DCMP$initFunc <- function(xData,yData) {
        c(runif(1),max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-abs(rnorm(1,mean = 0,sd = 0.1)),5*runif(1))
}
DCMP$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
DCMP$iter <- 100
DCMP$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
}



DCMN <- NULL
DCMN$name <- 'DCMN'
DCMN$symbol <- -1
DCMN$modelFunction <- decayModelFunction
DCMN$nParameters <- 4
DCMN$formula <- y ~ a +(b*exp(c*x^d))
DCMN$plotFunction <- plotCurve_tweise_single
DCMN$flag <-  -1
DCMN$initFunc <- function(xData,yData) {
        
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-max(yData),-abs(rnorm(1,mean = 0,sd = 0.1)),-abs(5*runif(1)))
        
}
DCMN$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
DCMN$iter <- 100
DCMN$printFormula <- function(ppar){
        paste(as.character(ppar[1]),"+","(",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*FEs^",as.character(ppar[4]),"))"
              ,sep = "")
        
}






# model 2.1 LogyLogx linear model
EPMP <- NULL
EPMP$name <- 'EPMP'
EPMP$modelFunction <- expLinearModelFunction
EPMP$formula <- y~a+b*exp(c*log(x+d))
EPMP$nParameters <- 4
EPMP$plotFunction <- plotCurve_line_logy_logx3_single
EPMP$initFunc <- function(xData,yData) {
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),abs(max(yData)*rnorm(1)),-abs(rnorm(1,mean = 0,sd = 0.1)),5*runif(1))
}
EPMP$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
EPMP$iter <- 100
EPMP$flag <- 1
EPMP$printFormula <- function(ppar){
        #  paste("exp(",as.character(ppar[1]),"+",as.character(ppar[2]),"*log(FEs))" ,sep = "")
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*log(FEs+",as.character(ppar[4]),"))" ,sep = "")
}







EPMN <- NULL
EPMN$name <- 'EPMN'
EPMN$modelFunction <- expLinearModelFunction
EPMN$formula <- y~a+b*exp(c*log(x+d))
EPMN$nParameters <- 4
EPMN$flag <- -1
EPMN$plotFunction <- plotCurve_line_logy_logx3_single
EPMN$initFunc <- function(xData,yData) {
        c(max(yData)-max(yData)/3+2*max(yData)/15*rnorm(1),-max(yData),runif(1),rnorm(1))
}
EPMN$nlsstart <- function(startlist){
        list(a = startlist[1],b = startlist[2],c = startlist[3],d = startlist[4])
}
EPMN$iter <- 100
EPMN$printFormula <- function(ppar){
        #  paste("exp(",as.character(ppar[1]),"+",as.character(ppar[2]),"*log(FEs))" ,sep = "")
        paste(as.character(ppar[1]),"+",as.character(ppar[2]),"*exp(",as.character(ppar[3]),"*log(FEs+",as.character(ppar[4]),"))" ,sep = "")
}
