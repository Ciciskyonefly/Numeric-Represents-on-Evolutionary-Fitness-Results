rm(list = ls())
source("./call_NN.R")
source("./call_LM.R")

start.time = Sys.time()

list_model = list(
                    expLinearModelpositive, expLinearModelnegative
                    ,logisticModelpositive
                    ,decayModelpositive, decayModelnegative
                    ,gompertzModelpositive, gompertzModelnegative
                  )

#list_model <- list(expLinearModelpositive)


volume = c(100)
iter = c(10)

maxTrain <- c(50)
maxTest <- c(100)

#RunNN(PRE_OR_NOT = "not", problem = "bbob", maxTrain = maxTrain, maxTest = maxTest)

RunLM(PRE_OR_NOT = "pre", problem = "tsp", predict.data = volume, maxTrain = maxTrain, maxTest = maxTest, list_model = list_model, iter = iter, option = "all")


end.time = Sys.time()


cat("cosume time :", end.time - start.time, "\n")