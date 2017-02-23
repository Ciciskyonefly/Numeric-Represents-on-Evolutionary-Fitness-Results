source("./problembased-funcs/LM.R")
source("./problembased-funcs/LM_model_func.R")
source("./base-funcs/path_config.R")


RunLM <- function(PRE_OR_NOT, problem = "maxsat", maxTr = c(50), maxTe = c(100), list.model, iter = c(10), option = "each"){
    
    path.config <- path_config(problem)
    file.path <- path.config[1]
    save.path <- path.config[2]
    
    if(!exists(save.path))
        dir.create(save.path)
    
    for(it in 1:length(iter)){
        if(grepl("pre", PRE_OR_NOT)){
            save.path <- paste(save.path, "maxTrain-maxTest/", sep = "")
        }
        if(!exists(save.path))
            dir.create(save.path)
        if(option == "each"){
            
            for(mod in 1:length(list_model)){
                cat("model each:",unlist(list_model[mod])$name,"\n")
                for(maxtrain in 1:length(maxTr)){
                    cat("Maxtrain : ", maxTr[maxtrain], "\n")
                    for(maxtest in 1:length(maxTe)){
                        cat("Maxtest : ", maxTe[maxtest], "\n")
                        
                        mainfunc(
                            PRE_OR_NOT = PRE_OR_NOT,
                            maxTrain = maxTr[maxtrain],
                            maxTest =  maxTe[maxtest],
                            iter = iter[it], 
                            list_model = list.model[mod], 
                            file.path = file.path, 
                            save.path = save.path, 
                            cal.error.method = "BL"
                        )
                        
                    }
                    
                }
                
            }
            
        } else {
            
            for(maxtrain in 1:length(maxTr)){
                cat("Maxtrain : ", maxTr[maxtrain], "\n")
                for(maxtest in 1:length(maxTe)){
                    
                    cat("model all:\n")
                    mainfunc(
                        PRE_OR_NOT = PRE_OR_NOT,
                        maxTrain = maxTr[maxtrain],
                        maxTest =  maxTe[maxtest],
                        iter = iter[it], 
                        list_model = list.model, 
                        file.path = file.path, 
                        save.path = save.path, 
                        cal.error.method = "BL"
                    )
                }
            }
        }
        
    }
    
}







