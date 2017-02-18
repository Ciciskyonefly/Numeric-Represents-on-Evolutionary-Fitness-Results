library(data.table)
library(Matrix)
library(dplyr)
library(ggplot2)
library(stringr)


order.Instance <- function(instances){
        # Reorder instances order.
        #
        #
        library(stringr)
        instances <- unique(instances)
        numIns = str_extract_all(instances,"[0-9]+")
        x <- do.call(rbind,numIns)
        dataOrderMatrix <-cbind(as.vector(instances),unlist(numIns))
        colnames(dataOrderMatrix) <- c("datafile","num")
        dataOrderMatrix = dataOrderMatrix[order(dataOrderMatrix[,2]),]
        return(dataOrderMatrix[,"datafile"])
}

 
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


load<-function(file.path = "./BenchMarking/modelresults/tspResult/250_gompertzModelpositives_model.csv"){
        data.table = read.csv(file.path,header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        #if(length(which(data.table[,"residuals"] > 1000)) > 0)
        #        data.table = data.table[-which(data.table[,"residuals"] > 1000),]
        #data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        #     data.matrix = as.matrix(data.table)
        return(data.table)
}






compare_up_bound_change <- function(file.path,size = 10 ){
        
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
        merge.data <- NULL
        for(i in 1:length(list.name)){
                
                data.path = paste(file.path,list.name[i],sep = "")
                #      cat(list.name[i],"\n")
                para.matrix <- load(data.path)
                if(i > 1){
                        merge.data <- merge(merge.data, para.matrix, by = c("instance_file", "model"))   
                } else {
                        merge.data <- para.matrix
                }
                
        }
        return (merge.data)
        
}


file.path = "./BenchMarking/modelresults/LM.tsp/"
size = 10
compare.data <- compare_up_bound_change(file.path)

compare.data$instance_file <- droplevels(compare.data$instance_file)
exract.names <- do.call(rbind,strsplit(as.vector(compare.data$instance_file),"/"))
compare.data$instances <- factor(exract.names[,3],ordered = TRUE,levels = order.Instance(exract.names[,3]))
compare.data$algorithms <- factor(exract.names[,1],ordered = TRUE,levels = order.Algorithm(exract.names[,1]))   
compare.data <- compare.data[with(compare.data,order(algorithms,instances)), ]

# compare a,b changes before and after set up-bound in group_b algorithms
b.compare.data <- compare.data[compare.data$algorithms %in% c("ts0r0b","ts10r10b","ts1e7r0b"), ]
b.compare.data$algorithms <- droplevels(b.compare.data$algorithms)
by_algorithms <- group_by(b.compare.data,instances)
a.b.aver <- summarise(by_algorithms, round(mean(a.x),4), round(mean(a.y),4), round(mean(b.x),4), round(mean(b.y),4))
write.csv(a.b.aver, file = "a.b.change.csv", row.names = FALSE)

## compare a,b changes before and after set up-bound in group_f algorithms
f.compare.data <- compare.data[compare.data$algorithms %in% c("ts0r0f","ts10r10f","ts1e7r0f"), ]
f.compare.data$algorithms <- droplevels(b.compare.data$algorithms)
by_algorithms <- group_by(b.compare.data,instances)
a.b.aver <- summarise(by_algorithms, round(mean(a.x),4), round(mean(a.y),4), round(mean(b.x),4), round(mean(b.y),4))
write.csv(a.b.aver, file = "a.b.change.csv", row.names = FALSE)





#compare residuals changes before and after set up-bound in group_f algorithms
compare.data$var = (round(compare.data$residuals.x,4) != round(compare.data$residuals.y,4))
changes.data <- compare.data[compare.data$var == TRUE, ]
changes.data$instance_file <- droplevels(changes.data$instance_file)
exract.names <- do.call(rbind,strsplit(as.vector(changes.data$instance_file),"/"))
changes.data$instances <- factor(exract.names[,3],ordered = TRUE,levels = order.Instance(exract.names[, 3]))
changes.data$algorithms <- factor(exract.names[,1],ordered = TRUE,levels = order.Algorithm(exract.names[, 1]))     
names(changes.data)
GG <- summary(select(changes.data,starts_with("residuals")))
write.csv(GG, file = "residual.csv",row.names = FALSE)


select(changes.data,starts_with("a"))
show.B <- select(changes.data,instance_file,starts_with("b"),instances,algorithms)
show.B <- show.B[with(show.B,order(algorithms,instances)), ]
select(show.B,instances,b.x,b.y)



