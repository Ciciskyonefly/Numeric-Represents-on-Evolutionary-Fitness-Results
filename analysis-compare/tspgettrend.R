rm(list = ls())

load<-function(file.path = "./modelresults/LM.tsp.pre/10_gompertzModelpositive_model.csv"){
        data.table = read.csv(file.path,header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        if(length(which(data.table[,"residuals"] > 1000)) > 0)
                data.table = data.table[-which(data.table[,"residuals"] > 1000),]
        data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        #     data.matrix = as.matrix(data.table)
        return(data.table)
}



order.Instance <- function(instances){
        library(stringr)
        instances <- unique(instances)
        numIns = str_extract_all(instances,"[0-9]+")
        x <- do.call(rbind,numIns)
        dataOrderMatrix <-cbind(as.vector(instances),unlist(numIns))
        colnames(dataOrderMatrix) <- c("datafile","num")
        dataOrderMatrix = dataOrderMatrix[order(dataOrderMatrix[,2]),]
        return(dataOrderMatrix[,"datafile"])
}

# 
order.Algorithm <- function(algorithm){
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




SaveMeanPara <- function(para.matrix,algorithm,save.path = "./modelresults/tsp.c.d/",i){
        # Compute the mean value according to instances or algorithms
        #
        # Args:
        #   para.matrix: A  data.frame of data you want to caculate
        #   algorithm: A factor you want to evaluate, must be one columu of the data.frame
        #
        # Returns:
        #    No return.
        #    Save two files. One is the average value, one is the range value.
        
        
        
        
        range.a <- tapply(para.matrix$a, list(algorithm), range)
        range.b <- tapply(para.matrix$b, list(algorithm), range)
        range.c <- tapply(para.matrix$c, list(algorithm), range)
        
        
        mean.a <- tapply(para.matrix$a, list(algorithm), mean)
        mean.b <- tapply(para.matrix$b, list(algorithm), mean)
        mean.c <- tapply(para.matrix$c, list(algorithm), mean)
        
        
        
        range.a <-matrix(as.numeric(unlist(range.a)),ncol = 2, byrow = TRUE)
        range.b <-matrix(as.numeric(unlist(range.b)),ncol = 2, byrow = TRUE)
        range.c <-matrix(as.numeric(unlist(range.c)),ncol = 2, byrow = TRUE)
        if(ncol(para.matrix) == 8){
                range.d <- tapply(para.matrix$d, list(algorithm), range)
                mean.d <- tapply(para.matrix$d, list(algorithm), mean)
                range.d <-matrix(as.numeric(unlist(range.d)),ncol = 2, byrow = TRUE)
        }

        
    
        if(ncol(para.matrix) == 8){
                
                mean.matrix <- cbind(mean.a,mean.b,mean.c,mean.d)
                save.rangedecay <- do.call(cbind,list(range.a,range.b,range.c,range.d))
              #  cat(save.rangedecay)
                algorithm <- droplevels(algorithm)
                rownames(save.rangedecay) <- levels(algorithm)
                colnames(save.rangedecay) <- c("a$min","a$max","b$min","b$max","c$min","c$max","d$min","d$max")
                mean.name <- paste(save.path,as.character(para.matrix$model[1]),"_",i,"_mean.csv",sep = "")
                range.name <- paste(save.path,para.matrix$model[1],"_",i,"_range.csv",sep = "")
            #    cat(mean.name)
                
            #    print(mean.matrix)
                write.csv(file = mean.name,round(mean.matrix,4))
                range.name
                write.csv(file = range.name,round(save.rangedecay,4))
                
        }else{
                
                mean.matrix <- cbind(mean.a,mean.b,mean.c)
                save.rangedecay <- do.call(cbind,list(range.a,range.b,range.c))
                algorithm <- droplevels(algorithm)
                rownames(save.rangedecay) <- levels(algorithm)
                colnames(save.rangedecay) <- c("a$min","a$max","b$min","b$max","c$min","c$max")
                mean.name <- paste(save.path,para.matrix$model[1],"_",i,"_mean.csv",sep = "")
                range.name <- paste(save.path,para.matrix$model[1],"_",i,"_range.csv",sep = "")
                write.csv(file = mean.name,mean.matrix)
                write.csv(file = range.name,round(save.rangedecay,4))
                
        }
        
}





f_loop <- function(file.path,size = 250 ){
        
        list.name = list.files(file.path)
        csv.list= grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }
              
        
        size.list = grep(size,list.name)
        
        if(length(size.list)==0){
                cat("no useful data.")
                break
        }else
                list.name = list.name[size.list]
        
        for(i in 1:length(list.name)){
                
                data.path = paste(file.path,list.name[i],sep = "")
          #      cat(list.name[i],"\n")
                cat(data.path)
                para.matrix <- load(data.path)
                if(is.null(para.matrix)||nrow(para.matrix) == 0)
                        next
                strsplit.instance_file <- strsplit(as.vector(para.matrix$instance_file),"/")
                instance_file.split <- do.call(rbind,strsplit.instance_file)
                para.matrix[,"algorithm"] <- instance_file.split[,1]
                para.matrix[,"instances"] <- instance_file.split[,3]
                #head(para.matrix)
                fac.instances <- factor(para.matrix$instances,ordered = TRUE,levels = order.Instance(para.matrix[,"instances"]))
                fac.algorithm <- factor(para.matrix$algorithm,ordered = TRUE,levels = order.Algorithm(para.matrix$algorithm))
                para.matrix[,"instances"] <- fac.instances
                para.matrix[,"algorithm"] <- fac.algorithm

                SaveMeanPara(para.matrix,para.matrix$algorithm,i = "para.matrix_1")
                
                
                
                SaveMeanPara(para.matrix,para.matrix$instances,i = "para.matrix_2")

                library(stringr)
                numIns = str_extract_all(para.matrix$instances,"[0-9]+")
                x <- do.call(rbind,numIns)
                para.matrix[,"id"] <- as.numeric(x)
                para.matrix <- para.matrix[order(para.matrix$id),]
                para.matrix [,"id"] <- NULL

                b.matrix <- do.call(rbind,split(para.matrix,para.matrix$algorithm)[1:3])
                f.matrix <- do.call(rbind,split(para.matrix,para.matrix$algorithm)[4:6])
                SaveMeanPara(b.matrix,b.matrix$instances,i = "b.matrix_2")
                SaveMeanPara(f.matrix,f.matrix$instances,i = "f.matrix_2")
        }
        
        
}


file.path = "./modelresults/LM.tsp.pre/100percentleft/"
size = 10
#data.path = file.path
f_loop(file.path,size = 10)


