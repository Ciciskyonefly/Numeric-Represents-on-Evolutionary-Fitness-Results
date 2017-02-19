rm(list = ls())
source("./call_LM.R")
library(dplyr)

#args[1] problem 
#args[2] pre or not
#args[3] maxTrain
#args[4] maxTest
#args[5] all or each
args = commandArgs(trailingOnly=TRUE)

list_model = list_model = list(
    expLinearModelpositive, expLinearModelnegative
    ,logisticModelpositive
    ,decayModelpositive, decayModelnegative
    ,gompertzModelpositive, gompertzModelnegative
)

#args = c("tsp", "pre", "c(10)", "c(100,1000, 10000)", "all")
RunLM(problem = args[1], PRE_OR_NOT = args[2], maxTrain = eval(parse(text = args[3])) , maxTest = eval(parse(text = args[4])), option = args[5],
      predict.data = c(100), list_model = list_model, iter = c(10))

