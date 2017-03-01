rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/path_config.R")


#path_config
plotbehavior <- function(file, problem){
    library(dplyr)
    #extract save.path from file.
    res.dat <- read.csv(file)
    rawdata.path <- path_config(problem)[1]
    restore.path <- path_config(problem)[2]
    
    pdf.name <- gsub(".csv", ".pdf", file)
    pdf(pdf.name)
    par(mfrow = c(2, 2))
    for(fi in 1:nrow(res.dat)){
        
        rawdata.name <- gsub("/", "_", res.dat[fi, "instance_file"] %>% as.character)
        rawdata <- paste(rawdata.path, rawdata.name, ".csv", 
                         sep = "") %>% read.csv()
        xData <-  rawdata$x
        par <- res.dat[fi, c("a", "b", "c", "d")] %>% as.numeric
        eval(parse(text = res.dat$model[fi] %>% as.character))$plotFunction(rawdata, par, nrow(rawdata))
        title(main = res.dat[fi, "instance_file"] %>% as.character)
    }
    
    dev.off()
}




plotbehavior_maxTrain_maxTest <- function(file, problem){
    
    library(dplyr)
    #extract save.path from file.
    res.dat <- read.csv(file)
    rawdata.path <- path_config(problem)[1]
    restore.path <- path_config(problem)[2]
    
    tmp.string <- strsplit(file, "_") %>% unlist()
    maxTrain <-  tmp.string[2]%>%as.numeric()
    maxTest <-  tmp.string[3]%>%as.numeric()
    
    pdf.name <- gsub(".csv", ".pdf", file)
    pdf(pdf.name)
    par(mfrow = c(2, 2))
    for(fi in 1:nrow(res.dat)){
        
        rawdata.name <- gsub("/", "_", res.dat[fi, "instance_file"] %>% as.character)
        rawdata <- paste(rawdata.path, rawdata.name, ".csv", 
                         sep = "") %>% read.csv()
        
        test.script <- which(rawdata$x <= maxTest)
        if(length(test.script) !=0){
            raw.data <- rawdata[c(1: max(test.script)), ]
        } else {
            next
        }
        #train data
        train.script <- which(rawdata$x <= maxTrain)
        if(length(train.script) != 0){
            sample.index <- c(1: max(train.script))
        } else {
            next
        }
        
        par <- res.dat[fi, c("a", "b", "c", "d")] %>% as.numeric
        eval(parse(text = res.dat$model[fi] %>% as.character))$plotFunction(raw.data, par, sample.index)
        title(main = res.dat[fi, "instance_file"] %>% as.character)
    }
    
    dev.off()
}



library(dplyr)
path <- "./modelresults/LM.bbob.pre/100percentleft/"
res.list <- list.files(path)
res.list <- paste(path, res.list[grep("BL_.+.csv", res.list)], sep = "")



for(lm in 1:length(res.list)){
    plotbehavior(res.list[lm], "bbob")
}
