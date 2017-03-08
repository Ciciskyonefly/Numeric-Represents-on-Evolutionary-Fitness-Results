NN_path_config <- function(problem){
    
    if(grepl("maxsat", problem)){
        file.path = "./rawdata/progress-of-future-predict-maxsat/"
        save.path = "./modelresults/NN.maxsat.pre/maxTrain-maxTest/"
        if(!exists(save.path))
            suppressWarnings(dir.create(save.path))
    }
    
    if(grepl("tsp", problem)){
        file.path = "./rawdata/tsp-singleRun/"
        save.path = "./modelresults/NN.tsp.pre/singleRun/"
        if(!exists(save.path))
            suppressWarnings(dir.create(save.path))
    }
    
    if(grepl("bbob", problem)){
        file.path = "./rawdata/bbob-1-log-y/"
        save.path = "./modelresults/NN.bbob.pre/bbob-1-log-y/"
        if(!file.exists(save.path)){
            suppressWarnings(dir.create(save.path))
        }
    }
    
    return(c(file.path, save.path))
}