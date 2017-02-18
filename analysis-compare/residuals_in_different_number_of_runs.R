
# Choose the best number of runs

rm(list = ls())
source("./LM.R")

list_model <- list(logisticModelpositive,logisticModelnegative,
                   decayModelpositive,decayModelnegative,
           #        gompertzModelpositive,gompertzModelnegative,
                   expLinearModelpositive,expLinearModelnegative)


collect_data <- function(file.path = "./BenchMarking/modelresults/LM.maxsat/"){
        
        
        list.name = list.files(file.path)
        csv.list= grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }
        
        collect.data <- NULL
        collect.rownames <- NULL
        for(i in 1:length(list_model)){
                
                
                script = grep(unlist(list_model[i])$name,list.name)
                if(length(script) == 0) next
                
                collect.rownames <- cbind(collect.rownames, unlist(list_model[i])$name)
                
                temp.list.name <- list.name[script]
                library(stringr)
                #   cat(unique(str_extract(csv.list,"[0-9]+")))
                frame.names <-  unique(str_extract(temp.list.name,"[0-9]+"))
                residuals.frame <- NULL
                for(j in 1:length(script)){
                        
                        table.name <- paste(file.path,temp.list.name[j],sep = "")
                        temp.data <- read.csv(table.name)
                        names(temp.data) <- names(temp.data)[c(2,1,3:ncol(temp.data))]
                        residuals.frame <- cbind(residuals.frame,temp.data$residuals)
        
                }
                
                colnames(residuals.frame) <- frame.names
                residuals.frame <- residuals.frame[,c(1,6,2:5)]
                frame.names <- colnames(residuals.frame)    
                
                runs.count <- NULL
                for(i in  1:nrow(residuals.frame)){
                        runs.count <- rbind(runs.count,names(which.min(round(residuals.frame[i,], 5))))
                }
                
                temp.vector <- rep(0, length(frame.names))
                names(temp.vector) <- frame.names
                
                temp.vector[names(table(runs.count))] <- as.numeric(table(runs.count))
                
                
                collect.data <- rbind(collect.data, temp.vector)
                
                
        }
        
        
        rownames(collect.data) <- collect.rownames
        print (collect.data)
        write.csv(collect.data,file = "./compare_method_1/maxsat_numberofruns.csv")
#        
        
        
}


collect_data()






