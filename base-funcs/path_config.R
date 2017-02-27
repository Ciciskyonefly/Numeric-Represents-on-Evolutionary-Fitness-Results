path_config <- function(problem){
    # This function used for set rawdata path(file.path) and result save path(save.path)
    #
    #
    if(grepl("maxsat", problem)){
        file.path = "./rawdata/progress-of-future-predict-maxsat/"
        save.path = "./modelresults/LM.maxsat.pre/"
    }
    
    if(grepl("tsp", problem)){
        file.path = "./rawdata/tsp-singleRun/"
        save.path = "./modelresults/LM.tsp.pre/singleRun/"
    }
    if(grepl("bbob", problem)){
        file.path = "./rawdata/bbob-log-y/"
        save.path = "./modelresults/LM.bbob.pre/bbob-log-y/100percentleft/"
    }
    
    
    return (c(file.path, save.path))
    
}


