OrderAlgorithms.maxsat <- function(uni.alg){
        # Arrange algorithms by algorithms stratagy
        # 
        # Args:
        #   uni.alg: A vector or a list of algorithms need to be ordered.
        #
        # Return:
        #   A vector or list of algorithms.
        
        uni.alg <- unique(uni.alg) %>% sort()
        uni.alg.Crs = uni.alg[grep("Crs", uni.alg)]
        uni.alg.noCrs = uni.alg[-grep("Crs", uni.alg)]
        uni.alg = c(uni.alg.noCrs, uni.alg.Crs)
        
        return (uni.alg)
}

OrderInstance.tsp <- function(instances){
        library(stringr)
        instances <- unique(instances)
        numIns = str_extract_all(instances,"[0-9]+")
        x <- do.call(rbind,numIns)
        dataOrderMatrix <-cbind(as.vector(instances),unlist(numIns))
        colnames(dataOrderMatrix) <- c("datafile","num")
        dataOrderMatrix = dataOrderMatrix[order(dataOrderMatrix[,2]),]
        return(dataOrderMatrix[,"datafile"])
}

OrderAlgorithms.tsp <- function(algorithm){
        # Reorder algorithms name 
        #
        # Args:
        #   algorithm: A vector of list names.
        
        uni.algorithm <- unique(algorithm)
        uni.algorithm <- sort(uni.algorithm)
        uni.algorithm.0b = uni.algorithm[grep("0b",uni.algorithm)]
        uni.algorithm.0f = uni.algorithm[grep("0f",uni.algorithm)]
        uni.algorithm = c(uni.algorithm.0b,uni.algorithm.0f)
        return(uni.algorithm)
}

LoadMaxsat<-function(file.path = "./modelresults/LM.maxsat/10_decayModelpositive_model.csv"){
        data.table = read.csv(file.path, header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        if(length(which(data.table[,"residuals"] == 1000000)) > 0)
                data.table = data.table[-which(data.table[,"residuals"] == 1000000),]
        #data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        #     data.matrix = as.matrix(data.table)
     #   names(data.table) <- names(data.table)[c(2,1,3:ncol(data.table))]
        data.matrix <- data.table
        uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms.maxsat()
        uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2] 
        instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
        data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
        data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)
        
        return(data.matrix)
}



LoadTsp<-function(file.path = "./modelresults/LM.tsp/1_y/10_gompertzModelpositive_model.csv"){
        data.table = read.csv(file.path,header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        #if(length(which(data.table[,"residuals"] > 1000)) > 0)
        #        data.table = data.table[-which(data.table[,"residuals"] > 1000),]
        #data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        data.matrix <- data.table
        uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/|-"))[, 1] %>% unique() %>% OrderAlgorithms.tsp()
        uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/|-"))[, 3] %>% OrderInstance.tsp()
        instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
        data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/|-"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
        data.matrix$instances <- do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/|-"))[, 3] %>% factor(levels = uni.datafile, ordered = TRUE)
        return(data.matrix)
}

LoadBbob <- function(file.path =  "./modelresults/LM.bbob.pre/100percentleft/10_all_model.csv"){
    
    library(dplyr)
    library(stringr)
    data.matrix <- read.csv(file.path)
    pre.length <- nrow(data.matrix)
    func <- do.call(rbind, strsplit(as.vector(data.matrix$instance_file), "/"))[, 2]
    func <- func %>% factor()
    func.level <- levels(func)[levels(func) %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric %>% order()]
    func <- factor(func, levels = func.level, ordered = TRUE)
    data.matrix$func <- func
    
    dim <- do.call(rbind, strsplit(as.vector(data.matrix$instance_file), "/"))[, 3]
    dim.level <- paste("DIM", str_extract_all(dim, "[0-9]+", simplify = TRUE) %>% unique %>% as.numeric %>% sort, sep = "")
    data.matrix$dim <- factor(dim, levels = dim.level, ordered = TRUE)
    
    data.matrix <- data.matrix[order(data.matrix$dim), ]
    data.matrix <- data.matrix[order(data.matrix$func), ]
    
    func.lab <- data.matrix$func %>% as.vector()
    #5 function labels.
    #1 f1:f5 2 f6:f9 3 f10:f14 4 f19:f17 5 f20:f24
    func.lab[which(func.lab %in% c("f1", "f2", "f3", "f4", "f5"))] <- 1
    func.lab[which(func.lab %in% c("f6", "f7", "f8", "f9"))] <- 2
    func.lab[which(func.lab %in% c("f10", "f11", "f12", "f13", "f14"))] <- 3
    func.lab[which(func.lab %in% c("f15", "f16", "f17", "f18", "f19"))] <- 4
    func.lab[which(func.lab %in% c("f20", "f21", "f22", "f23", "f24"))] <- 5
    
    func.lab <- factor(func.lab)
    data.matrix$func.lab <- func.lab
    return(data.matrix)
}

