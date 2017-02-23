rm(list = ls())
source("./call_NN.R")
source("./call_LM.R")

start.time = Sys.time()

list_model = list(
                    EPMP, EPMN
                    ,LGMP, LGMN
                    ,DCMP, DCMN
                    ,GPMP, GPMN
                  )


volume = c(100)
iter = c(10)

maxTrain <- c(50)
maxTest <- c(100)


RunLM(PRE_OR_NOT = "pre", problem = "tsp",  maxTr = maxTrain, maxTe = maxTest, list.model = list_model, iter = iter, option = "each")


end.time = Sys.time()


cat("cosume time :", end.time - start.time, "\n")