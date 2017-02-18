rm(list = ls())
#library(data.table)
library(dplyr)
iter = c(20)
# iter = c(10) 
# maxiter = c(50)
# hiddensize  = c(6, 8, 10)
maxiter = c(50, 100, 150, 200, 250)
hiddensize = c( 6, 8, 10)

file_path = "./modelresults/NN.maxsat.pre/100percentleft/"
list.allfiles <- list.files(file_path)

subscript <- grep("residuals_.+.csv$", list.allfiles)
list.csvs <-  list.allfiles[subscript]

# it = 1
# hid = 1
# maxi = 1


for(it in 1:length(iter)){
        
        
        for (hid in 1:length(hiddensize)){
                
                error_influence_on_maxiter = NULL 
                algorithms_instances_names = NULL 
                
                for(maxi in 1: length(maxiter)){
                
                    #corresponding files.                
                    patter.String <-  paste(iter[it], "iter_", hiddensize[hid], "hiddensize_", maxiter[maxi], "maxiter", sep = "" )
                
                    file.sub <- patter.String %>% grep(list.csvs)    
                    if( file.sub %>% length() == 0 ) {
                            next
                    }
                    
                    caculate.list <-  list.csvs[file.sub]
                    
                    compare.data <- NULL
                    residuals <- NULL
                    for(run in 1: length(caculate.list)){
                            
                          temp.data = paste(file_path, caculate.list[run],sep = "") %>% read.csv(head = TRUE)
                          names(temp.data) <- c("alogorithms_instances_names", "residuals")
                          residuals <- cbind(temp.data$residuals, residuals)
                          algorithms_instances_names <- temp.data$alogorithms_instances_names
                            
                    }
                    
                    averageresiduals <- apply(residuals, 1, mean) %>% data.frame()
                    print(nrow(residuals))
                    cat(averageresiduals %>% length(), "\n")
                    names(averageresiduals) <- paste(iter[it], "iter_", hiddensize[hid], "hiddensize_", maxiter[maxi], "maxiter", sep = "")
                    
                    if(maxi == 1){
                            error_influence_on_maxiter = cbind(algorithms_instances_names,averageresiduals)
                    } else {
                            error_influence_on_maxiter = error_influence_on_maxiter %>% cbind(averageresiduals)
                    }    
                }
                
                savecsv.name = paste("./modelresults/NN.analysing/",iter[it],"iter_", hiddensize[hid], "hiddensize.csv", sep = "")
                error_influence_on_maxiter %>% write.csv(file = savecsv.name, row.names = FALSE)
                
                
        }
        
 #       rm(file_path)
}
