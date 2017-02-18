source("./problembased-funcs/LM.R")




RunLM <- function(PRE_OR_NOT, problem = "maxsat", maxTrain = c(50), maxTest = c(100), predict.data = c(100), list_model, iter = c(10), option = "each"){
  
  
  #base files
  if(grepl("maxsat", problem)){
    file.path = "./rawdata/maxsat/"
    save.path = "./modelresults/LM.maxsat.pre/"
    if(!exists(save.path))
      dir.create(save.path)
    cat("Maxsat Problem: \n")
  }
  
  if(grepl("tsp", problem)){
    file.path = "./rawdata/tsp-singleRun/"
    save.path = "./modelresults/LM.tsp.pre/singleRun/"
    if(!exists(save.path))
      dir.create(save.path)
    cat("Tsp Problem: \n")
  }
  if(grepl("bbob", problem)){
    file.path = "./rawdata/bbob-log-y/"
    save.path = "./modelresults/LM.bbob.pre/bbob-log-y/"
    if(!file.exists(save.path)){
        dir.create(save.path)
    }
    cat("BBob Problem: \n")
  }
  
  
  
  
  source("./problembased-funcs/LM_model_func.R")
  for(temp_i in 1:length(iter)){
    
    #   
    volume <- predict.data
    for (i in 1:length(volume)){
      # print(volume)
      cat(volume[i]/100)
      
      if(grepl("pre", PRE_OR_NOT)){
        every.savepath <- paste(save.path, "maxTrain-maxTest/", sep = "")
      } else {
        inner.filename = paste(volume,"percentleft",sep = "")
        every.savepath = paste(save.path,inner.filename[i],"/",sep = "")
        print(every.savepath)
      }
      
      # is file exists.
      if(!exists(every.savepath))
        dir.create(every.savepath)
      
      if(option == "each"){
        
        for(temp_j in 1:length(list_model)){
          cat("model each:",unlist(list_model[temp_j])$name,"\n")
          for(maxtrain in 1:length(maxTrain)){
            cat("Maxtrain : ", maxTrain[maxtrain], "\n")
            for(maxtest in 1:length(maxTest)){
              cat("Maxtest : ", maxTest[maxtest], "\n")
              
              mainfunc(
                PRE_OR_NOT = PRE_OR_NOT,
                maxTrain = maxTrain[maxtrain],
                maxTest =  maxTest[maxtest],
                iter[temp_i], 
                list_model[temp_j], 
                file.path, 
                every.savepath, 
                datavolume = volume[i]/100,
                method = 1
              )
              
            }
            
          }
          
        }
        
      } else {
        
        for(maxtrain in 1:length(maxTrain)){
          cat("Maxtrain : ", maxTrain[maxtrain], "\n")
          for(maxtest in 1:length(maxTest)){
            
            cat("model all:\n")
            mainfunc(
              PRE_OR_NOT = PRE_OR_NOT,
              maxTrain = maxTrain[maxtrain],
              maxTest =  maxTest[maxtest],
              iter[temp_i], 
              list_model, 
              file.path, 
              every.savepath, 
              datavolume = volume[i]/100,
              method = 1
            )
          }
        }
      }
      
    }
    
    rm(file_path)
  }
  
}






