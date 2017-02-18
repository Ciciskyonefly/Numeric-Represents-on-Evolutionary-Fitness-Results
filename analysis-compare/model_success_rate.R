
rm(list = ls())
source("./LM.R")

list_model <- list(logisticModelpositive,logisticModelnegative,
                   decayModelpositive,decayModelnegative,
                   gompertzModelpositive,gompertzModelnegative,
                   expLinearModelpositive,expLinearModelnegative)

SR <- 4
success_rate_funcion<-function(file.path = "./modelresults/tspResult/",save.path = "./modelresults/tsp_Analysing/", SR = 3){
        # Compute each models' successful rates by using model's residuals.
        # And save as .csv file.
        #
        # Args:
        #  file.path: The path where we can get model's results.
        #  save.path: The path where we save the successful rates we get.
        #
        # No return:
        #  Save the file.
        
        
        csv.list = list.files(file.path)  
        if(length(grep("10.+csv",list.files(file.path)))!=0)
                csv.list = csv.list[grep("10_.+csv", csv.list)]  # Filter .csv result file.
        
        #print(csv.list)
        library(stringr)
       
        #rate_collect <- unique(str_extract(csv.list,"[0-9]+"))
        #
        rate_collect <- c("10")
        name.list <- NULL
        
        collect.table <- NULL
        
        
        
        for(i in 1:length(list_model)){
                
                script = grep(unlist(list_model[i])$name,csv.list)
                if(length(script) == 0) next
                
                i_model_list = csv.list[script]
                rate.table = NULL
                itername <- do.call(rbind,strsplit(i_model_list,"_"))[,1]
                for(j in 1:length(i_model_list)){
                        data.table = read.csv(paste(file.path,i_model_list[j],sep = ""),header = TRUE)
                        names(data.table) = names(data.table)[c(2,1,3:ncol(data.table))]
                        pre.nrow = nrow(data.table)
                        if(length(which(data.table[,"residuals"] > SR)) > 0)
                                data.table = data.table[-which(data.table[,"residuals"] > SR), ]
             
                        
                        after.nrow = nrow(data.table)
                        success.rate = after.nrow/pre.nrow
                        rate.table = cbind(rate.table,cbind(round(success.rate,4)))
                }
                
                collect.table <- rbind(collect.table,rate.table)
                name.list <- rbind(name.list,unlist(list_model[i])$name)
                
                
                
                colnames(rate.table) <- rate_collect
                rownames(rate.table) <- unlist(list_model[i])$name
                
              #  save_ = paste(save.path,unlist(list_model[i])$name,".csv",sep = "")
            #    write.csv(rate.table,save_,row.names = FALSE)  
                
                
        }
        
        colnames(collect.table) <- rate_collect
        rownames(collect.table) <- name.list
        print (collect.table)
        
  #      write.csv(collect.table,save.path)
}


# 
# merge_success_rate <- function(.file){
#         
#         csv.list = list.files(file.path)  
#         if(length(grep(".csv",list.files(file.path)))!=0)
#                 csv.list = csv.list[grep(".csv", csv.list)]
#                 
# }

######################## 
maxsat_file_path = "./modelresults/LM.maxsat.pre/100percentleft/"
maxsat_save_path ="./analysis-compare/maxsat_success_rate.csv"
tsp_file_path = "./modelresults/LM.tsp.pre/100percentleft/"
tsp_save_path = "./analysis-compare/tsp_success_rate.csv"
cat("Matsat Problem:\n")
success_rate_funcion(maxsat_file_path, maxsat_save_path, SR)
cat("TSP Problem:\n")
success_rate_funcion(tsp_file_path, tsp_save_path, SR)


NN.maxsat.data <- read.csv("./modelresults/NN.maxsat.pre/100percentleft/residuals_20iter_6hiddensize_50maxiter_1runs.csv")
NN.maxsat.data <- NN.maxsat.data[, 2]
judge.length <- which(NN.maxsat.data > SR) %>% length()
if(judge.length == 0){
  cat("NN maxsat SR: 1 \n")
}else {
  success_rate <- (length(NN.maxsat.data) - judge.length)/length(NN.maxsat.data)
  cat("NN maxsat SR: ", success_rate, "\n")
}

NN.tsp.data <- read.csv("./modelresults/NN.tsp.pre/100percentleft/residuals_20iter_6hiddensize_50maxiter_1runs.csv")
NN.tsp.data <- NN.tsp.data[, 2]
judge.length <- which(NN.tsp.data > SR) %>% length()

if(judge.length == 0){
  cat("NN tsp SR: 1 \n")
}else {
  success_rate <- (length(NN.tsp.data) - judge.length)/length(NN.tsp.data)
  cat("NN tsp SR: ", success_rate, "\n")
}
