
getTheMatrixData_TspSuite <- function(filePath){
    # Read TSP problem every instance data
    # Args:
    #   filePath: The path of one instance.
    #
    # Return:
    #   A matrix of one TSP instance' fitness data.
    
    
    a = list.files(filePath)
    
    dirname = paste(filePath,"/",a[1],sep="")
    con <- file(dirname, "r") #read by line
    line = readLines(con,n=1)
    line=readLines(con,n=1)
    merge.data = as.vector(unlist(strsplit(line,"\\t"))) #list to vector
    while( length(line) != 0 ) {
        line=readLines(con, n=1)
        if(grepl("SECTION_END", line) == TRUE){
            break
        }else{
            line = strsplit(line,"\\t")
            line = as.vector(unlist(line))
            merge.data = rbind(merge.data, line, deparse.level = 0)
        }
    }
    close(con)
    #############
    
    
    
    #############
    for(i in 2:length(a)){
        dirname = paste(filePath,"/",a[i],sep="")
        
        con <- file(dirname, "r")
        line = readLines(con,n=1)
        line=readLines(con,n=1)
        while( length(line) != 0 ) {
            line=readLines(con,n=1)
            if(grepl("SECTION_END",line) == TRUE){
                break
            }else{
                line = strsplit(line,"\\t")
                line = as.vector(unlist(line))
                merge.data = rbind(merge.data,line,deparse.level = 0)
            }
        }
        close(con)
    }
    
    
    s = merge.data[, 1:6] # Extract data matrix
    s[, 3] = s[, 6]
    s = s[, 1:3]
    s=matrix(as.numeric(s), nrow=nrow(s)) # Turn to numeric matrix
    return (s)
} 




getMatrixData_Maxsat<-function(filePath){
    # Read maxsat problem data
    # Args:
    #   filePath: The path of one instance.
    #
    # Return:
    #   A matrix of one maxsat instance' fitness results data.
    
    
    a = list.files(filePath) 
    m = length(a)
    merge.data = read.table(file=paste(filePath,a[1],sep = ''),header = FALSE,sep='')
    for(j in 2:m){
        new.data = read.table(file = paste(filePath,a[j],sep = ''),header= FALSE,sep='')
        merge.data = rbind(merge.data,new.data)
    }
    merge.data = as.matrix(merge.data)
    merge.data = merge.data[order(merge.data[, 1]), ]
    return (merge.data)
}




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


getTheMatrixData_BBOB_singleRun <- function(file.path, run.times = 1, alg.name, dat.name, save.path){
    
    dat <- read.csv(file.path, header = TRUE)
    runs.head.line <- grep("function evaluation", dat[, 1] %>% as.character) 
    num.runs <- (runs.head.line %>% length() + 1)
    
    
    count.i = 1
    save.dat <- NULL
    for(i in 1:num.runs){
        
        if(i == 1) {
            tmp.dat <-  strsplit(dat[1:(runs.head.line[i] - 1), ] %>% as.character, " ")
        } else if(i == num.runs){
            tmp.dat <-  strsplit(dat[(runs.head.line[i-1] + 1) : nrow(dat), ] %>% as.character, " ")
        } else{
            tmp.dat <-  strsplit(dat[(runs.head.line[i-1]+1) : (runs.head.line[i] - 1) , ] %>% as.character, " ")
        }
        single.save.dat <- do.call(rbind, tmp.dat)[, c(1, 3)] %>% data.frame
        
        # cat(is.null(single.save.dat), "\n")
        # print(save.dat)
        if(ncol(single.save.dat) == 1){
            single.save.dat <- t(single.save.dat)
        }
        colnames(single.save.dat) <- c("x", "y")
        
        save.dat <- save.dat %>% rbind(single.save.dat)
        #cat("single.save.dat:", names(single.save.dat), "\n")
        #cat("save.dat:", names(save.dat), "\n")
        # save.dat <- save.dat %>% rbind(single.save.dat) 
        # cat("i ", i%%run.times, "\n")
        if(i %% run.times == 0){
            #cat(ncol(save.dat), "\n")
            colnames(save.dat) <- c("x", "y")
            save.dat <- save.dat[order(save.dat$x), ]
            save.datname = gsub("bbobexp_","", dat.name) 
            save.file = paste(save.path,
                              alg.name,"_", gsub(".dat", "", save.datname), "-", count.i, ".csv", sep = "")
            cat(save.file, "\n")
            write.csv(save.dat, file = save.file, row.names = FALSE)
            save.dat <- NULL
            count.i <- count.i + 1
            
        }
    }
}
