rm(list = ls())
source("./call_NN.R")
source("./call_LM.R")

start.time = Sys.time()

list_model = list(
                    GPMP, LGMP, 
                    EPMP, DCMP,
                    EPMN, LGMN,
                    DCMN, GPMN
)



iter <- 10


# RunLM(PRE_OR_NOT = "not", problem = "bbob",  list.model = list_model,
#        method = "BL", iter = iter, option = "each")

RunNN(PRE_OR_NOT = "not", problem = "bbob")


end.time = Sys.time()


cat("cosume time :", end.time - start.time, "\n")