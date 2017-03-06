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
        y <- log10(raw.dat$y)
        x <- raw.dat$x
        transf.dat <- data.frame(x, y)
        if(which(is.infinite(y)) %>% length() != 0)
            transf.dat <- transf.dat[-which(is.infinite(y)), ]
        transf.dat <- transf.dat[order(transf.dat$x), ]
        write.csv(transf.dat, paste(save.path, list.file[dat], sep = ""), row.names = FALSE)
    }
    
}



# log_y_dat <- read.csv(paste(save.path, "CMAES_f5_DIM10.csv", sep = ""))
# dat <- read.csv(paste(file.path, "CMAES_f5_DIM10.csv", sep = ""))
file.path = "./rawdata/multiple-run-dat/bbob-15-run/"
save.path = "./rawdata/multiple-run-dat/bbob-15-run-log-y/"
if(!file.exists(save.path)){
    dir.create(save.path)
}

transform_bbob_log_y_data(file.path, save.path)