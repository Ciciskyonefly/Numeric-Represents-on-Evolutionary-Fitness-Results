rm(list = ls())

transform_bbob_log_y_data <- function(file.path, save.path){
    library(dplyr)
    list.file <- file.path %>% list.files()
    dat <- 1

    for(dat in 1:length(list.file)){
        raw.dat <- read.csv(paste(file.path, list.file[dat], sep = ""))
        
        if(which(raw.dat$y<=0) %>% length !=0){
            raw.dat <- raw.dat[-which(raw.dat$y<=0), ]
        }
        y <- log10(raw.dat$y + 1)
        x <- raw.dat$x
        transf.dat <- data.frame(x, y)
        if(which(is.infinite(y)) %>% length() != 0)
            transf.dat <- transf.dat[-which(is.infinite(y)), ]
        transf.dat <- transf.dat[order(transf.dat$x), ]
        write.csv(transf.dat, paste(save.path, list.file[dat], sep = ""), row.names = FALSE)
    }
    
}


transform_bbob_log_y_1_data <- function(file.path , save.path){
    if(!file.exists(save.path)){
        dir.create(save.path)
    }
    file.list <- list.files(file.path)
    
    for(i in 1:length(file.list)){
        file <- paste(file.path, file.list[i], sep = "")
        dat <- read.csv(file)
        dat$y <- log10(dat$y + 1)
        save.file <- paste(save.file.path, file.list[i], sep = "")
        write.csv(dat, save.file, row.names = FALSE)
    }
    
}


file.path = "./rawdata/bbob/"
save.path = "./rawdata/bbob-1-log-y/"
if(!file.exists(save.path)){
   suppressWarnings(dir.create(save.path))
}
transform_bbob_log_y_data(file.path, save.path)