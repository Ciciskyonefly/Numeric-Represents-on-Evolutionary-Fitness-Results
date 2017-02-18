 rm(list = ls())

library(data.table)
library(dplyr)


file.path = "./modelresults/NN.maxsat.pre/100percentleft/"

list.names <- list.files(file.path) 

final.path <- list.names[grep("residuals_", list.names)]

library(stringr)

order.script <-  do.call(rbind, strsplit(final.path, "_"))[, 3] %>% str_extract("[0-9]+") %>% as.numeric() %>% order()
final.path <- final.path[order.script]
hidden.names <- do.call(rbind, strsplit(final.path, "_"))[, 3]
read.data <- NULL
names <- c("instance_alogorithm_name")


for( i in 1: length(final.path)){
        
        path <- paste(file.path, final.path[i], sep = "")
        temp_data <- read.csv(path)
       
        
        read.data <-if( is.null(read.data) ){
                temp_data
        } else {
                read.data %>% cbind(temp_data[, 2])
        }
        
        names <- names %>% cbind(hidden.names[i])
     #   print(head(read.data)) 
        
}

names(read.data) <- names
head(read.data)


residuals.values <- select(read.data, matches("hiddensize"))
xx <- apply(residuals.values, 1, which.min) %>% data.frame()

xx$. %>% factor(levels = c(1,2,3,4,5), labels = names(residuals.values)) %>% table()  %>%  data.table() %>% print()


