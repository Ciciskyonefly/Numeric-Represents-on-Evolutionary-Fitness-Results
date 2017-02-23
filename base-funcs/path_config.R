path_config <- function(problem){
    # This function used for set rawdata path(file.path) and result save path(save.path)
    #
    #
    if(grepl("maxsat", problem)){
        file.path = "./rawdata/maxsat/"
        save.path = "./modelresults/LM.maxsat.pre/"
        cat("Maxsat Problem: \n")
    }
    
    if(grepl("tsp", problem)){
        file.path = "./rawdata/tsp-singleRun/"
        save.path = "./modelresults/LM.tsp.pre/singleRun/"
        cat("Tsp Problem: \n")
    }
    if(grepl("bbob", problem)){
        file.path = "./rawdata/bbob-log-y/"
        save.path = "./modelresults/LM.bbob.pre/bbob-log-y/"
        cat("BBob Problem: \n")
    }
    
    
    return (c(file.path, save.path))
    
}


