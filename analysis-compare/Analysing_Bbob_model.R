rm(list = ls())
library("BB")
#include RMSE
library(qpcR)
library(Formula)
library(stringr)
library(dplyr)

condition.10 <- c("data_f3","data_f4","data_f13","data_f15","data_f17","data_f20")
condition.100 <- c("data_f4","data_f7","data_f16","data_f24")
condition.1000 <- c("data_f18","data_f21","data_f22")
condition.1E6 <- c("data_f2","data_f10","data_f11","data_f12")

condition.list <- list(condition.10,condition.100,condition.1000,condition.1E6)


{"r:function-01:OrderStringNum"}
OrderStringNum <- function(stringNum){
        
        stringNum <- unique(stringNum)
        uni <- unlist(unique(str_extract_all(stringNum,"[0-9]+")))
        uni.data <- cbind(uni,stringNum,deparse.level = 0)
        uni.data <-uni.data[order(as.numeric(uni.data[,1])),]
        orderStringNum <- uni.data[,2]
        return(orderStringNum)
}


{"r:function-02:unidataOrderDIM"}
unidataOrderDIM<- function(data.txt){
        unidata.txt <- unique(data.txt)
        unidata.DIM <- NULL
        for(i in 1:length(unidata.txt)){
                temp =  unlist(strsplit(unidata.txt[i],"_"))
                unidata.DIM = rbind(unidata.DIM,temp[3],deparse.level = 0)
        }
        unidata.DIM <- unique(unidata.DIM)
        unidata.DIM <- OrderStringNum(unidata.DIM)
        return (unidata.DIM)
}

{"r:function-03:generareTableMatrix"}
generateTableMatrix <- function(algorithmMatrix){
        
        #################################################
        ####THE MATRIX WHICH SAVING RESULTS
        #################################################
        resMatrix = matrix(0,11,ncol(algorithmMatrix)-2)
        rownames(resMatrix) <- c("Med","IQR","Mean","Stddev","CV(%)","Min","Q0.05","Q0.25","Q0.75","Q0.95","Max")
        #colnames(resMatrix) <- c("a","b","c","d")
        colnames(resMatrix) <- c("a","b")
        
        #write.csv(resMatrix,"D:/MatrixTest.csv")
        #################################################
        
        CV <- function(x){
                return (100*sd(x)/mean(x))
        }
        
        Q0.05<- function(x){
                return (as.vector(quantile(x,probs = 0.05)))
        }
        
        Q0.25 <- function(x){
                return (as.vector(quantile(x,probs = 0.25)))
                
        }
        
        Q0.75 <- function(x){
                return (as.vector(quantile(x,probs = 0.75)))
                
        }
        Q0.95 <- function(x){
                return (as.vector(quantile(x,probs = 0.95)))
        }
        numericAlgorithmMatrix = matrix(as.numeric(algorithmMatrix[,-c(1,2)]),nrow(algorithmMatrix),ncol(algorithmMatrix)-2)
        numericAlgorithmMatrix
        resMatrix["Med",] = apply(numericAlgorithmMatrix, 2, median)
        resMatrix["IQR",] = round(apply(numericAlgorithmMatrix, 2, IQR),4)
        resMatrix["Mean",] = apply(numericAlgorithmMatrix, 2, mean)
        resMatrix["Stddev",] = apply(numericAlgorithmMatrix, 2, sd)
        resMatrix["CV(%)",] = apply(numericAlgorithmMatrix, 2, CV)
        resMatrix["Min",] = apply(numericAlgorithmMatrix, 2, min)
        resMatrix["Q0.05",] = apply(numericAlgorithmMatrix, 2, Q0.05)
        resMatrix["Q0.25",] = apply(numericAlgorithmMatrix, 2, Q0.25)
        resMatrix["Q0.75",] = apply(numericAlgorithmMatrix, 2, Q0.75)
        resMatrix["Q0.95",] = apply(numericAlgorithmMatrix, 2, Q0.95)
        resMatrix["Max",] = apply(numericAlgorithmMatrix, 2, max)
        return (resMatrix)
}

{"r:function-04:generateIntegerTableMatrix"}
generateIntegerTableMatrix <- function(algorithmMatrix){
        return (generateTableMatrix(algorithmMatrix))
}

{"r:function-05:get condition data_f"}
data_f.condition <- function(){
        condition.table = read.table(file = "../R Code/BenchMarking/OPResults/data_f_conditioning.csv",header = TRUE, fileEncoding = "utf-8",sep = ",")
        condition.matrix = as.matrix(condition.table)
        condition.matrix <- condition.matrix[complete.cases(condition.matrix),]
        #排序
        condition.matrix <- condition.matrix[order(as.numeric(condition.matrix[,2])),]
        return(condition.matrix)
}


load<-function(){
        data.table = read.csv("../R Code/BenchMarking/OPResults/bbobResult/test_logisticModel_bbob_10_14.csv",header = TRUE,sep = ",")
        pre.length = nrow(data.table)
        print (pre.length)
        colnames(data.table)
        if(length(which(data.table[,"residuals"] > 10000)) > 0)
                data.table = data.table[-which(data.table[,"residuals"] > 10000),]
        data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        data.matrix = as.matrix(data.table)
        return(data.matrix)
}

getBbobAlgorithmName <- function(algorithm){
        
        algorithm <- unique(algorithm)
        uni.algorithm <- NULL
        for(i in 1:length(algorithm)){
                if(is.na(unlist(str_split(algorithm[i],"_"))[2]))
                        uni.algorithm <- cbind(uni.algorithm,algorithm[i],deparse.level = 0)
                else
                        uni.algorithm = cbind(uni.algorithm,unlist(str_split(algorithm[i],"_"))[2],deparse.level = 0)
        }
        return (uni.algorithm)
}




{"r:main"}
data.matrix <- load()
data.matrix[,"instance_file"] = paste(data.matrix[,"instance_file"],"/",sep="")
################################提取算法，实例，数据名
file = data.matrix[,"instance_file"]
head(file)
algorithm = NULL
datafile = NULL
data.txt = NULL
for(i in 1:length(file)){
        temp =  unlist(strsplit(file[i],"/"))
        algorithm = rbind(algorithm,temp[1],deparse.level = 0)
        datafile = rbind(datafile,temp[2],deparse.level = 0)
        data.txt = rbind(data.txt,temp[3],deparse.level = 0)
}

count_by_algorithm <- data.frame(as.factor(algorithm),rep(1,length(algorithm)))
ALGORITHM_success_num <- aggregate(count_by_algorithm[,2], by=list(count_by_algorithm[,1]), FUN=sum)
ALGORITHM_success_num <- plyr::rename(ALGORITHM_success_num, c("Group.1"="ALGORITHM", "x"="Success_Instance_Num"))

fiter_dim <- gsub("bbobexp_f._","",data.txt)%>%gsub("bbobexp_f.._","",.)
count_by_dim <- data.frame(as.factor(fiter_dim),rep(1,length(algorithm)))
DIM_success_num <- aggregate(count_by_dim[,2], by=list(count_by_dim[,1]), FUN=sum)
DIM_success_num <- plyr::rename(DIM_success_num, c("Group.1"="DIM", "x"="Success_Instance_Num"))

write.csv(DIM_success_num,"../R Code/BenchMarking/OPResults/bbob_Analysing/logisticModel_DIM_success_num.csv",row.names = FALSE)

write.csv(ALGORITHM_success_num,"../R Code/BenchMarking/OPResults/bbob_Analysing/logisticModel_ALGORITHM_success_num.csv",row.names = FALSE)


uni.algorithm = getBbobAlgorithmName(algorithm)
unidatafile = OrderStringNum(datafile)
unidata.txt = unidataOrderDIM(data.txt)


{"r:main-plug2-para in different data_f with same DIM"}
pdfname = paste("../R Code/BenchMarking/OPResults/bbob_Analysing/test_logisticModel_DIM.pdf")
pdf(pdfname)
algorithmMatrix = NULL
par(mfrow = c(3,ncol(data.matrix)-2))


for(j in 1:length(unidatafile)){
        
        pattern.DATA = paste(unidatafile[j],"/",sep = "")
        DATA.tempMatrix = data.matrix[grep(pattern.DATA,data.matrix[,"instance_file"]),]
        if(!is.matrix(DATA.tempMatrix))
                DATA.tempMatrix = t(as.matrix(DATA.tempMatrix))
        for(k in 1:length(unidata.txt)){
                pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
                if(length(grep(pattern.DIM,DATA.tempMatrix[,"instance_file"])) == 0)
                        next
                
                DIM.tempMatrix = DATA.tempMatrix[grep(pattern.DIM,DATA.tempMatrix[,"instance_file"]),]
                if(!is.matrix(DIM.tempMatrix))
                        DIM.tempMatrix = t(as.matrix(DIM.tempMatrix))
                
                tempMatrix = matrix(0,ncol = ncol(data.matrix) - 2,nrow = length(uni.algorithm),byrow = TRUE)
                
                for(i in 1:length(uni.algorithm)){
                        pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
                        if(length(grep(pattern.ALG,DIM.tempMatrix[,"instance_file"])) == 0)
                                next
                        else
                                tempMatrix[i,] = DIM.tempMatrix[grep(pattern.ALG,DIM.tempMatrix[,"instance_file"]),c(3:ncol(data.matrix))]
                        
                }
                
                tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(uni.algorithm),byrow = FALSE)
                
                
                
                if(ncol(tempMatrix) == 4){
                        colnames(tempMatrix) = c("a","b","c","d")
                        model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
                }else if(ncol(tempMatrix) == 3){
                        colnames(tempMatrix) = c("a","b","c")
                        model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
                }
                
                for(pp in 1:ncol(tempMatrix)){
                        
                        x = c(1:nrow(tempMatrix))
                        y = tempMatrix[,pp]
                        plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
                        axis(1,1:nrow(tempMatrix),uni.algorithm,las = 2)
                        axis(2)
                        titleString = paste(unidatafile[j],"-",unidata.txt[k],"-",colnames(tempMatrix)[pp],sep = "")
                        title(main = titleString,col.main = "RED" )
                        box()
                        grid()
                }
        }
}
dev.off()



# {"r:main-plug1-every data_f's DIM para change"}
# 
# algorithmMatrix = NULL
# par(mfrow = c(3,ncol(data.matrix)-2))
# 
# for(i in 1:length(uni.algorithm)){
#         pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
#         algorithm.matrix = data.matrix[grepl(pattern.ALG,data.matrix[,"instance_file"]),]
#         if(is.null(algorithm.matrix)) next
#         if(!is.matrix(algorithm.matrix)) 
#                 algorithm.matrix = t(as.matrix(algorithm.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
#         
#         for(j in 1:length(unidatafile)){
#                 patterString = paste(unidatafile[j],"/",sep = "")
#                 datafile.matrix  = algorithm.matrix[grepl(patterString,algorithm.matrix[,"instance_file"]),]
#                 
#                 if(is.null(datafile.matrix))
#                         next
#                 
#                 if(!is.matrix(datafile.matrix)){
#                         datafile.matrix = t(as.matrix(datafile.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
#                 }
#                 
#                 
#                 tempMatrix = matrix(0,ncol = ncol(data.matrix)-2,nrow = length(unidata.txt),byrow = TRUE)    
#                 for(k in 1:length(unidata.txt)){
#                         pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
#                         if(length(grep(pattern.DIM,datafile.matrix[,"instance_file"])) == 0)
#                                 next
#                         else
#                                 tempMatrix[k,] = datafile.matrix[grep(pattern.DIM,datafile.matrix[,"instance_file"]),c(3:ncol(data.matrix))]
#                 }
#                 
#                 tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidata.txt),byrow = FALSE)
#                 if(ncol(tempMatrix) == 4){
#                         colnames(tempMatrix) = c("a","b","c","d")
#                         model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
#                 }else if(ncol(tempMatrix) == 3){
#                         colnames(tempMatrix) = c("a","b","c")
#                         model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
#                 }
#                 
#                 for(pp in 1:ncol(tempMatrix)){
#                         
#                         x <- c(2,3,5,10,20,40)
#                         y = tempMatrix[,pp]
#                         plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
#                         unidata.aixs = paste("DIM",c(1:40),sep = "")
#                         axis(1,at= x,labels = unidata.aixs[x],las = 2)
#                         axis(2)
#                         titleString = paste(uni.algorithm[i],"_",unidatafile[j],"_",colnames(tempMatrix)[pp],sep = "")
#                         title(main = titleString,col.main = "RED" )
#                         box()
#                         grid()
#                 }
#         }
# }
# 
# dev.off()
# 




{"r:main-plug5-DIM with different data_f under same algorithm"}
algorithmMatrix = NULL
#par(mfrow = c(3,ncol(data.matrix)-2))
for(cl in 1:length(condition.list)){
        unidatafile = condition.list[[cl]]
        pdfname1 = paste("../R Code/BenchMarking/OPResults/bbob_Analysing/logisticModel-data_f-BOBB_",cl,".pdf",sep = "")
        pdf(pdfname1)
        par(mfrow = c(3,3))
        for(k in 1:length(unidata.txt)){
                
                for(i in 1:length(uni.algorithm)){
                        
                        tempMatrix = matrix(0,ncol = ncol(data.matrix) - 2,nrow = length(unidatafile),byrow = TRUE)
                        
                        for(j in 1:length(unidatafile)){
                                
                                pattern.DATA = paste(unidatafile[j],"/",sep = "")
                                if(length(grep(pattern.DATA,data.matrix[,"instance_file"])) == 0)
                                        next
                                DATA.tempMatrix = data.matrix[grep(pattern.DATA,data.matrix[,"instance_file"]),]
                                if(!is.matrix(DATA.tempMatrix))  
                                        DATA.tempMatrix = t(as.matrix(DATA.tempMatrix))
                                
                                
                                pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
                                if(length(grep(pattern.DIM,DATA.tempMatrix[,"instance_file"])) == 0)
                                        next
                                DIM.tempMatrix = DATA.tempMatrix[grep(pattern.DIM,DATA.tempMatrix[,"instance_file"]),]
                                if(!is.matrix(DIM.tempMatrix))  
                                        DIM.tempMatrix = t(as.matrix(DIM.tempMatrix))
                                
                                pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
                                if(length(grep(pattern.ALG,DIM.tempMatrix[,"instance_file"])) == 0)
                                        next
                                else
                                        tempMatrix[j,] = DIM.tempMatrix[grep(pattern.ALG,DIM.tempMatrix[,"instance_file"]),c(3:ncol(data.matrix))]
                                
                        }
                        
                        tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidatafile),byrow = FALSE)
                        
                        
                        
                        if(ncol(tempMatrix) == 4){
                                colnames(tempMatrix) = c("a","b","c","d")
                                model.ylim = matrix(c(min(tempMatrix[,1] -10),max(tempMatrix[,1] +10),min(tempMatrix[,2] -1),max(tempMatrix[,2] + 1),min(tempMatrix[,3] - 1),max(tempMatrix[,3] + 1),min(tempMatrix[,4]-1),max(tempMatrix[,4]) + 1),ncol = 2,byrow = TRUE)
                        }else if(ncol(tempMatrix) == 3){
                                colnames(tempMatrix) = c("a","b","c")
                                model.ylim = matrix(c(min(tempMatrix[,1]),max(tempMatrix[,1]),min(tempMatrix[,2] -1),max(tempMatrix[,2] + 1),min(tempMatrix[,3] - 1),max(tempMatrix[,3] + 1)),ncol = 2,byrow = TRUE)
                        }
                        
                        for(pp in 1:ncol(tempMatrix)){
                                
                                x = c(1:nrow(tempMatrix))
                                y = tempMatrix[,pp]
                                plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = c(1:length(x)),ylim = model.ylim[pp,])
                                axis(1,1:nrow(tempMatrix),unidatafile)
                                axis(2)
                                titleString = paste(unidata.txt[k],"-",uni.algorithm[i],"-",colnames(tempMatrix)[pp],sep = "")
                                title(main = titleString,col.main = "RED" )
                                box()
                                grid()
                        }
                }
        }
        
        dev.off()
}




{"r:main-plug6-bbob data_f's conditioning"}
pdfname.conditioning = "../R Code/BenchMarking/OPResults/bbob_Analysing/new_logisticModel_conditioning.pdf"
pdf(pdfname.conditioning)
#取消科学计数法
options(scipen = 200)
algorithmMatrix = NULL
par(mfrow = c(3,3))
condition.matrix <- data_f.condition()

for(k in 1:length(unidata.txt)){
        
        for(i in 1:length(uni.algorithm)){
                
                tempMatrix = matrix(NA,ncol = ncol(data.matrix) - 2,nrow = nrow(condition.matrix),byrow = TRUE)
                
                for(j in 1:length(condition.matrix[,1])){
                        
                        pattern.DATA = paste(condition.matrix[j,1],"/",sep = "")
                        if(length(grep(pattern.DATA,data.matrix[,"instance_file"])) == 0)
                                next
                        DATA.tempMatrix = data.matrix[grep(pattern.DATA,data.matrix[,"instance_file"]),]
                        if(!is.matrix(DATA.tempMatrix))  
                                DATA.tempMatrix = t(as.matrix(DATA.tempMatrix))
                        
                        
                        pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
                        if(length(grep(pattern.DIM,DATA.tempMatrix[,"instance_file"])) == 0)
                                next
                        DIM.tempMatrix = DATA.tempMatrix[grep(pattern.DIM,DATA.tempMatrix[,"instance_file"]),]
                        if(!is.matrix(DIM.tempMatrix))  
                                DIM.tempMatrix = t(as.matrix(DIM.tempMatrix))
                        
                        pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
                        if(length(grep(pattern.ALG,DIM.tempMatrix[,"instance_file"])) == 0)
                                next
                        else
                                tempMatrix[j,] = DIM.tempMatrix[grep(pattern.ALG,DIM.tempMatrix[,"instance_file"]),c(3:ncol(data.matrix))]
                        
                }
                
                tempMatrix = matrix(as.numeric(tempMatrix),nrow = nrow(condition.matrix),byrow = FALSE)
                if(ncol(tempMatrix) == 4){
                        colnames(tempMatrix) = c("a","b","c","d")
                        model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
                        tempMatrix[is.na(tempMatrix[,1]),1] <- 1
                        tempMatrix[is.na(tempMatrix[,2]),2] <- 0
                        tempMatrix[is.na(tempMatrix[,3]),3] <- 0
                        tempMatrix[is.na(tempMatrix[,4]),4] <- 0
                }else if(ncol(tempMatrix) == 3){
                        colnames(tempMatrix) = c("a","b","c")
                        model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
                        tempMatrix[is.na(tempMatrix[,1]),1] <- 1
                        tempMatrix[is.na(tempMatrix[,2]),2] <- 0
                        tempMatrix[is.na(tempMatrix[,3]),3] <- 0
                }
                
                for(pp in 1:ncol(tempMatrix)){
                        
                        x.value = as.numeric(condition.matrix[,2])
                        y.value = tempMatrix[,pp]
                        ylab.str = paste(colnames(tempMatrix)[pp],"-values",sep = "")
                        if(pp == 1){
                                plot(x.value, y.value,pch = 4,axes = FALSE,log = "xy",cex = 1.5,col = c(1:nrow(condition.matrix)),xlab = "function_condition",ylab = ylab.str)
                                #       legend(5000,1000000,condition.matrix[,1],col=c(1:nrow(condition.matrix)),pch = 4,ncol = 1,cex = 0.6)
                        }else{
                                plot( x.value, y.value,pch = 4,axes = FALSE,log = "x",cex = 1.5,col = c(1:nrow(condition.matrix)),xlab = "function_condition",ylab = ylab.str)
                        }
                        axis(1,at = unique(x.value),labels = unique(x.value))
                        axis(2)
                        titleString = paste(unidata.txt[k],"-",uni.algorithm[i],"-",colnames(tempMatrix)[pp],sep = "")
                        title(main = titleString,col.main = "RED" )
                        box()
                        grid()
                }
        }
}

dev.off()


