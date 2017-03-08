

RunNN <- function(PRE_OR_NOT, problem = "maxsat", maxTr = c(50), maxTe = c(100),  iter = c(20), maxiter = c(50) ,hiddensize = c(4)){
    
    source("./problembased-funcs/nn_model_func.R")
    source("./nn/NN_path_config.R")
    file.path <- NN_path_config(problem)[1]
    save.path <- NN_path_config(problem)[2]
    for(it in 1:length(iter)){
        
        #inner.filename = paste(volume, "percentleft", sep = "")
        #Fixpath
        
        if(grepl("pre", PRE_OR_NOT)){
            save.path <- paste(save.path, "maxTrain-maxTest/", sep = "")
        }
        if(!exists(save.path))
            suppressWarnings(dir.create(save.path))
        
        cat("iter: ", iter[it], "\n")
        for(maxi in 1: length(maxiter)){
            cat("maxiter: ", maxiter[maxi], "\n")
            for(hid in 1:length(hiddensize)){
                for(maxtrain in 1:length(maxTr)){
                    for(maxtest in 1:length(maxTe)){
                        
                        NN.mainfunc(
                            PRE_OR_NOT = PRE_OR_NOT,
                            maxTrain = maxTr[maxtrain],
                            maxTest = maxTe[maxtest],
                            maxiter = maxiter[maxi],
                            hiddensize = hiddensize[hid],
                            iter = iter[it], 
                            file.path  = file.path, 
                            save.path = save.path, 
                            method = 1,
                            runtime = 1
                        )
                        
                    }
                    
                }
                
            }
        }
        
        
        
    }
    
    rm(file.path)
}





