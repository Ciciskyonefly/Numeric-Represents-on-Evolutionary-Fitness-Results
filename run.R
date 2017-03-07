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



maxTrain <- c(10, 50, 100)
maxTest <- c(100)


 RunLM(PRE_OR_NOT = "not", problem = "tsp",  maxTr = maxTrain, maxTe = maxTest, list.model = list_model,
       method = "BL", iter = iter, option = "each")


#RunNN(PRE_OR_NOT = "not", problem = "bbob")

end.time = Sys.time()


cat("cosume time :", end.time - start.time, "\n")