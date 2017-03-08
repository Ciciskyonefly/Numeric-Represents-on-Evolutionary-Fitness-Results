path_config <- function(problem){
    # This function used for set rawdata path(file.path) and result save path(save.path)
    #
    #
    if(grepl("maxsat", problem)){
        file.path = "./rawdata/progress-of-future-predict-maxsat/"
        save.path = "./modelresults/LM.maxsat.pre/"
        if(!file.exists(save.path))
            suppressWarnings(dir.create(save.path))
    }
    
    if(grepl("tsp", problem)){
        file.path = "./rawdata/tsp/"
        save.path = "./modelresults/LM.tsp.pre/20-run/"
        if(!file.exists(save.path))
            suppressWarnings(dir.create(save.path))
    }
    if(grepl("bbob", problem)){
        file.path = "./rawdata/bbob-1-log-y-singleRun/"
        save.path = "./modelresults/LM.bbob.pre/bbob-1-log-y/"
        if(!file.exists(save.path))
            suppressWarnings(dir.create(save.path))
    }
    
    
    return (c(file.path, save.path))
    
}


