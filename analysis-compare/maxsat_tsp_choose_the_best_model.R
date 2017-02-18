
rm(list = ls())
library(scmamp)
source("./LM.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")


list_model <- list(logisticModelpositive,
                   logisticModelnegative,
                   decayModelpositive,decayModelnegative,
                   gompertzModelpositive,gompertzModelnegative,
                   expLinearModelpositive)



Getmodelnames <- function(list_model){
        #
        #
        #
        #
        #
        names <- NULL
        for(i in 1:length(list_model)){
                names = cbind(names,unlist(list_model[i])$name)
        }
        return(names)
}



success_rate_funcion<-function(file.path = "./modelresults/LM.tsp/",size = 250){
        # Compute each models' successful rates by using model's residuals.
        # And save as .csv file.
        #
        # Args:
        #  file.path: The path where we can get model's results.
        #  save.path: The path where we save the successful rates we get.
        #
        # No return:
        #  Save the file.
        
        list.name = list.files(file.path)
        csv.list= grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }
        
        
        
        size.list = grep(size,list.name)
        
        if(length(size.list)==0){
                cat("no useful data.")
                break
        }else
          list.name = list.name[size.list]
        
        
        if(length(list.name != 0))
                judge.names <- do.call(rbind,strsplit(list.name,"_"))[, 2]
        
        model.names = Getmodelnames(list_model)
        newmodel.names <- NULL
        residual.data <- NULL
        for(i in 1:length(list.name)){
                
                if(!(judge.names[i] %in% model.names))  next
                       
                      
                table.name <- paste(file.path,list.name[i],sep = "")
                temp.data <- read.csv(table.name)
                if(grep("experiment.xml",temp.data$instance_file %>% as.character) %>% length != 0)
                   temp.data = temp.data[-grep("experiment.xml", temp.data$instance_file %>% as.character), ]
         #       names(temp.data) <- names(temp.data)[c(2,1,3:ncol(temp.data))]
                
                  print(nrow(temp.data))
                newmodel.names <- cbind(newmodel.names,judge.names[i])
                residual.data <- cbind(residual.data,temp.data$residuals)
                
                
        }
        
        colnames(residual.data) <- newmodel.names
        print(head(residual.data))        
        
        return (residual.data)
        
}

file.path = "./modelresults/LM.tsp.pre/100percentleft/"
size = 10
data <- success_rate_funcion(file.path, size)


# NN.data <- read.csv("./modelresults/NN.tsp.pre/100percentleft/residuals_20iter_6hiddensize_50maxiter_1runs.csv")
# NN <- NN.data[, 2]
# data <- data %>% cbind(NN)
goodmodels <- NULL

for( i in 1:nrow(data)){
      goodmodels <- rbind(goodmodels,names(which(data[i,] == apply(data,1,min)[i])))        
}
freqency <- data.frame(table(factor(goodmodels)))
freqency %>% print()
# Maxsat
# 1     decayModelnegative   91
# 2     decayModelpositive   39
# 3 expLinearModelpositive  100
# 4  logisticModelpositive  277
# 5                     NN   93

#Maxat --no NN
# 1     decayModelnegative   98
# 2     decayModelpositive   80
# 3 expLinearModelpositive  110
# 4  logisticModelpositive  312


#tsp
#1 gompertzModelpositive    7
#2                    NN  119

#TSP
# 1    decayModelpositive    9
# 2 gompertzModelpositive  116
# 3 logisticModelpositive    1


#writeTabular(freqency,file = "./maxsat_best_model_freqency.csv",row.names = FALSE)
#write.csv(freqency,file = "./compare_method_1/LM_tsp_freqency.csv",row.names = FALSE)
