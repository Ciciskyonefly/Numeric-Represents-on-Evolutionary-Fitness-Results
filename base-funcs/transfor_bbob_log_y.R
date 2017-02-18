rm(list = ls())

transform_bbob_log_y_data <- function(file.path, save.path){
    library(dplyr)
    list.file <- file.path %>% list.files()
    dat <- 1
    
    pdfname <- "./bbob/bbob_log_y_plot.pdf"
    pdf(pdfname)
    par(mfrow = c(2, 2))
    
    for(dat in 1:length(list.file)){
        raw.dat <- read.csv(paste(file.path, list.file[dat], sep = ""))
        
        if(which(raw.dat$y<=0) %>% length !=0){
            raw.dat <- raw.dat[-which(raw.dat$y<=0), ]
        }
        y <- log10(raw.dat$y)
        x <- raw.dat$x
        transf.dat <- data.frame(x, y)
        plot(x, y, pch = 20, col = "blue")
        title(main = list.file[dat] )
        write.csv(transf.dat, paste(save.path, list.file[dat], sep = ""), row.names = FALSE)
    }
    
    dev.off()
}



# log_y_dat <- read.csv(paste(save.path, "CMAES_f5_DIM10.csv", sep = ""))
# dat <- read.csv(paste(file.path, "CMAES_f5_DIM10.csv", sep = ""))
file.path = "./rawdata/bbob/"
save.path = "./rawdata/bbob-log-y/"
if(!file.exists(save.path)){
    dir.create(save.path)
}

transform_bbob_log_y_data(file.path, save.path)