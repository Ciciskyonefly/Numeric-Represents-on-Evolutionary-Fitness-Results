

deal_data <- function(file.list, raw.path){
        
        res <- NULL
        for(i in 1:length(file.list)){
                
                #load data
                x <- read.csv(paste(raw.path, file.list[i], sep = ""))
                
                if(is.null(res)){
                        res <- head(x)
                } else {
                        res <- rbind(rbind(res, x))
                }
                
        }
        
        return (res)
}

library(foreach)
library(doParallel)
library(parallel)

# detect core numbers
core.num <- detectCores(logical = F)
cl <- makeCluster(core.num)
registerDoParallel(cl)

x <- list.files("./rawdata/tsp-part-singleRun/")
raw.path <- "./rawdata/tsp-part-singleRun/"

res <- foreach(x,.combine = rbind) %dopar% deal_data(x, raw.path)


