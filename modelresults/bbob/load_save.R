rm(list = ls())
library(dplyr)
alg.list <- list.files()

getTheMatrixData_BBOB <- function(filePath){
        con <- file(filePath, open = "r")
        line = readLines(con,n=1)
        merge.data = as.vector(unlist(strsplit(line,"\\|")))
        while( length(line) != 0 ) {
                line=readLines(con,n=1)
                if(grepl("%.*",line) == FALSE&&length(line) != 0 ){
                        line = strsplit(line," ")
                        line = as.vector(unlist(line))
                        merge.data = rbind(merge.data,line,deparse.level = 0)
                }
        }
        close(con)
        
        
        
        s = merge.data[,c(1,3)]
        s = s[-1, ]
        s = matrix(as.numeric(s),nrow=nrow(s)) %>% data.frame()
        s = s[order(s[,1]), ]
        colnames(s) = c("x", "y")
        return (s)
}  



for(alg in 1:length(alg.list)){
        func.list = alg.list[alg] %>% list.files()
        for(func in 1:length(func.list)){
                dat.list = paste("./", alg.list[alg], "/",func.list[func], sep = "") %>% list.files(pattern = ".*?\\.dat")
                for(dat in 1:length(dat.list)){
                        
                        filepath = paste("./", alg.list[alg], "/",func.list[func],"/", dat.list[dat], sep = "")
                        data = getTheMatrixData_BBOB(filepath)
                        
                        save.datname = gsub("bbobexp_","", dat.list[dat]) 
                       
                        save.file = paste("E:/Numeric-Represents-on-Evolution-Fitness-Results/code-r/rawdata/bbob/",
                                          alg.list[alg],"_",  gsub(".dat",".csv",save.datname), sep = "")
                        write.csv(data, file = save.file, row.names = FALSE)
                }
        }
        
}


