

RunNN <- function(PRE_OR_NOT, problem = "maxsat",maxTrain, maxTest, predict.data = c(100), iter = c(20), maxiter = c(50) ,hiddensize = c(6)){
  
  source("./problembased-funcs/nn_model_func.R")
  if(grepl("maxsat", problem)){
    file.path = "./rawdata/progress-of-future-predict-maxsat/"
    save.path = "./modelresults/NN.maxsat.pre/maxTrain-maxTest/"
    if(!exists(save.path))
      dir.create(save.path)
    cat("Maxsat Problem: \n")
  }
  
  if(grepl("tsp", problem)){
    file.path = "./rawdata/tsp-singleRun/"
    save.path = "./modelresults/NN.tsp.pre/singleRun/"
    if(!exists(save.path))
        dir.create(save.path)
    cat("Tsp Problem: \n")
  }
    
  if(grepl("bbob", problem)){
    file.path = "./rawdata/bbob-log-y/"
    save.path = "./modelresults/NN.bbob.pre/bbob-log-y/"
    if(!file.exists(save.path)){
        dir.create(save.path)
    }
    cat("BBob Problem: \n")
    
    
  }
  
  for(it in 1:length(iter)){
    volume <- predict.data
    for (i in 1:length(volume)){
      
      #inner.filename = paste(volume, "percentleft", sep = "")
      #Fixpath
        
        if(grepl("pre", PRE_OR_NOT)){
            every.savepath <- paste(save.path, "maxTrain-maxTest/", sep = "")
        } else {
            inner.filename = paste(volume,"percentleft",sep = "")
            every.savepath = paste(save.path,inner.filename[i],"/",sep = "")
            print(every.savepath)
        }
        
      if(!exists(every.savepath))
       dir.create(every.savepath)
      cat("iter: ", iter[it]," volume: ", volume[i], "\n")
      for(maxi in 1: length(maxiter)){
        cat("maxiter: ", maxiter[maxi], "\n")
        for(hid in 1:length(hiddensize)){
          
          for(maxtrain in 1:length(maxTrain)){
            
            for(maxtest in 1:length(maxTest)){
              
              NN.mainfunc(
                PRE_OR_NOT = PRE_OR_NOT,
                maxTrain = maxTrain[maxtrain],
                maxTest = maxTest[maxtest],
                maxiter = maxiter[maxi],
                hiddensize = hiddensize[hid],
                iter = iter[it], 
                file.path  = file.path, 
                save.path = every.savepath, 
                datavolume = volume[i]/100,
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
  
}






