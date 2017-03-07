rm(list = ls())
library("BB")
library("ggplot2")
#include RMSE
#library(qpcR)
library(Formula)
library(stringr)

##########################################################
# rearrange algorithms order
###########################################################
OrderAlgorithm <- function(unialgorithm){
        
        unialgorithm <- sort(unialgorithm)
        unialgorithm.0b = unialgorithm[grep("0b",unialgorithm)]
        unialgorithm.0f = unialgorithm[grep("0f",unialgorithm)]
        unialgorithm = c(unialgorithm.0b,unialgorithm.0f)
        return(unialgorithm)
}




OrderInstancesName <- function(unidatafile){
        # Reorder the benchmarking test instance names according to the scale of the city not dictionary order
        #
        # Args:
        #   unidatafile : A list of benchmarking instances name   
        #
        #
        library(stringr)
        
        #extract number from string name
        numIns = str_extract_all(unidatafile,"[0-9]+")
        dataOrderMatrix <-cbind(unidatafile,unlist(numIns))
        colnames(dataOrderMatrix) <- c("datafile","num")
        dataOrderMatrix = dataOrderMatrix[order(dataOrderMatrix[,2]),]
        return( dataOrderMatrix[,"datafile"])
}




load <- function(file.path){
        # Load the data we want to analyse
        #
        # Ags:
        #   file.path
        #
        # Returns:
        #   A matrix that store the data we are going to use.
        
        data.table = read.csv(file.path,header = TRUE,sep = ",")
        pre.length = nrow(data.table)
        colnames(data.table)
        if(length(which(data.table[,"residuals"] == 1000000)) > 0)
                data.table = data.table[-which(data.table[,"residuals"] == 1000000),]
        data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        data.matrix = as.matrix(data.table)
        return(data.matrix)
        
}



file.path = "./modelresults/LM.tsp.pre/100percentleft/10_gompertzModelpositive_model.csv"
data.matrix<- load(file.path)
file.list = data.matrix[,"instance_file"]
head(file.list)
algorithm = NULL
instancesNameList = NULL
#data.txt = NULL
for(i in 1:length(file.list)){
        temp =  unlist(strsplit(file.list[i],"/"))
        algorithm = rbind(algorithm,temp[1],deparse.level = 0)
        instancesNameList = rbind(instancesNameList,temp[3],deparse.level = 0)
}

uni.algorithm = OrderAlgorithm(unique(algorithm))
unidatafile = OrderInstancesName(unique(instancesNameList))


# ###################
# #small plug___4 // Analysing instance para under different algorithms
# ###################



Analysis1 <- function(data.matrix,save.path = "./BenchMarking/modelresults/tsp.analysing/"){
        
        # pdfname = paste(save.path,"_analysis_1.pdf",sep = "")
        # pdf(pdfname)
         instanceSize = unidatafile
        # par(mfrow = c(3,ncol(data.matrix)-2))
        
        for(i in 1:length(instanceSize)){
                algorithmMatrix = data.matrix[grepl(instanceSize[i],data.matrix[,"instance_file"]),]
                tempMatrix = matrix(0,length(uni.algorithm),ncol(algorithmMatrix)-2)
                for(j in 1:length(uni.algorithm)){
                        patterString = paste(uni.algorithm[j],"/",sep = "")
                        
                        
                        sssMatrix = algorithmMatrix[grepl(patterString,algorithmMatrix[,"instance_file"]),]
                        
                        if(!is.matrix(sssMatrix)){
                                sssMatrix = t(as.matrix(sssMatrix))
                                sssMatrix = sssMatrix[,-c(1:2)]
                                sssMatrix = t(as.matrix(sssMatrix))
                        }else
                                sssMatrix = sssMatrix[,-c(1:2)]
                        sssMatrix = matrix(as.numeric(sssMatrix),nrow(sssMatrix),ncol(sssMatrix))
                        tempMatrix[j,] = apply(sssMatrix, 2, mean)
                }
                
                if(ncol(tempMatrix) == 4){
                        colnames(tempMatrix) = c("a","b","c","d")
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.1),max(tempMatrix[,"a"]+0.1),
                                            min(tempMatrix[,"b"]-1),max(tempMatrix[,"b"]+1),
                                            min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"]+1),
                                            min(tempMatrix[,"d"] - 1),max(tempMatrix[,"d"]+1)),
                                          ncol = 2,byrow = TRUE)
                        
                }else if(ncol(tempMatrix) == 3){
                        colnames(tempMatrix) = c("a","b","c")
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.2),max(tempMatrix[,"a"]+ 0.2),min(tempMatrix[,"b"]-1),max(tempMatrix[,"b"]+1),min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"]+1)),
                                          byrow = TRUE,ncol = 2)
                }
                
                for(k in 1:ncol(tempMatrix)){
                        
                        setEPS()
                        names = paste(unidatafile[i],"_",colnames(tempMatrix)[k],".eps",sep = "")
                        postscript(names)
                        x = c(1:nrow(tempMatrix))
                        y = tempMatrix[,k]
                        #,ylim = model.ylim[k,]
                        plot(x,y,pch = 3,lwd= 2,axes = FALSE,xlab = " ",ylab = " ",col = "BLUE")
                        axis(1,1:nrow(tempMatrix),uni.algorithm)
                        axis(2)
                 #       titleString = paste(unidatafile[i],"_",colnames(tempMatrix)[k],sep = "")
                 #      title(main = titleString,col.main = "RED" )
                        box()
                        grid()
                        
                        dev.off()
                }
                
        }
        
        #dev.off()
}





reuni.alogorithm <- c( "ts0r0b","ts10r10b","ts1e7r0b","ts0r0f","ts10r10f","ts1e7r0f")

Analysis2 <- function(data.matrix,save.path = "./modelresults/tsp.analysing/" ){
        pdfname = paste(save.path,"_analysis_2.pdf",sep = "") 
        pdf(pdfname)
        algorithmMatrix = NULL
        instanceSize = unidatafile
        # reuni.alogorithm <- c( "ts10r10b","ts0r0b","ts1e7r0b","ts0r0f","ts10r10f","ts1e7r0f")
        par(mfrow = c(3,ncol(data.matrix)-2))
        
        for(i in 1:length(instanceSize)){
                algorithmMatrix = data.matrix[grepl(instanceSize[i],data.matrix[,"instance_file"]),]
                tempMatrix = matrix(0,length(reuni.alogorithm),ncol(algorithmMatrix)-2)
                for(j in 1:length(reuni.alogorithm)){
                        patterString = paste(reuni.alogorithm[j],"/",sep = "")
                        
                        
                        sssMatrix = algorithmMatrix[grepl(patterString,algorithmMatrix[,"instance_file"]),]
                        
                        if(!is.matrix(sssMatrix)){
                                sssMatrix = t(as.matrix(sssMatrix))
                                sssMatrix = sssMatrix[,-c(1:2)]
                                sssMatrix = t(as.matrix(sssMatrix))
                        }else
                                sssMatrix = sssMatrix[,-c(1:2)]
                        sssMatrix = matrix(as.numeric(sssMatrix),nrow(sssMatrix),ncol(sssMatrix))
                        tempMatrix[j,] = apply(sssMatrix, 2, mean)
                }
                
                if(ncol(tempMatrix) == 4){
                        colnames(tempMatrix) = c("a","b","c","d")
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.2),max(tempMatrix[,"a"]+0.2),
                                            min(tempMatrix[,"b"]-1),max(tempMatrix[,"b"]+1),
                                            min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"]+1),
                                            min(tempMatrix[,"d"] - 1),max(tempMatrix[,"d"]+1)),
                                          ncol = 2,byrow = TRUE)
                }else if(ncol(tempMatrix) == 3){
                        colnames(tempMatrix) = c("a","b","c")
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.2),max(tempMatrix[,"a"]+ 0.2),min(tempMatrix[,"b"]-1),max(tempMatrix[,"b"]+1),min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"]+1)),
                                          byrow = TRUE,ncol = 2)
                        
                }
                
                for(k in 1:ncol(tempMatrix)){
                        x = c(1:nrow(tempMatrix))
                        y = tempMatrix[,k]
                        #,ylim = model.ylim[k,]
                        plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE",ylim = exp.ylim[k,])
                        axis(1,1:nrow(tempMatrix),reuni.alogorithm)
                        axis(2)
                        titleString = paste(unidatafile[i],"_",colnames(tempMatrix)[k],sep = "")
                        title(main = titleString,col.main = "RED" )
                        box()
                        grid()
                }
        }
        
        dev.off()
}




Analysis3 <- function(data.matrix,save.path = "./BenchMarking/modelresults/tsp.analysing/"){
        # pdfname = paste(save.path,"_analysis_3.pdf",sep = "")
        # pdf(pdfname)
        #   par(mfrow = c(2,ncol(data.matrix)-2))
        #   par(mfrow = c(2,2))
        algorithmMatrix = NULL
        instanceSize = unidatafile
        uni.algorithm <- c("ts1e7r0b","ts1e7r0f")
        for(i in 1:length(uni.algorithm)){
                patterString = paste(uni.algorithm[i],"/",sep = "")
                algorithmMatrix = data.matrix[grepl(patterString,data.matrix[,"instance_file"]),]
                ########每个实例规模下的平均参数值矩阵
                #algorithmMatrix = algorithmMatrix[,-c(1:2)]
                tempMatrix = matrix(0,length(instanceSize),ncol(algorithmMatrix)-2)
                for(j in 1:length(instanceSize)){
                        sssMatrix = algorithmMatrix[grepl(instanceSize[j],algorithmMatrix[,"instance_file"]),]
                        if(is.null(sssMatrix)) continue
                        if(!is.matrix(sssMatrix)){
                                sssMatrix = t(as.matrix(sssMatrix))
                                sssMatrix = sssMatrix[,-c(1:2)]
                                sssMatrix = t(as.matrix(sssMatrix))
                        }else
                                sssMatrix = sssMatrix[,-c(1:2)]
                        
                        sssMatrix = matrix(as.numeric(sssMatrix),nrow(sssMatrix),ncol(sssMatrix))
                        tempMatrix[j,] = apply(sssMatrix, 2, mean)
                        # print(sssMatrix)
                        # print(tempMatrix[j,])
                }
                
                if(ncol(tempMatrix) == 4){
                        colnames(tempMatrix) = c("a","b","c","d")
                        
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.05),max(tempMatrix[,"a"]+0.05),
                                            min(tempMatrix[,"b"]-1),max(tempMatrix[,"b"]+1),
                                            min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"]+1),
                                            min(tempMatrix[,"d"] - 0.1),max(tempMatrix[,"d"]+ 0.1)),
                                          ncol = 2,byrow = TRUE)
                        print(exp.ylim)
                        
                }else if(ncol(tempMatrix) == 3){
                        colnames(tempMatrix) = c("a","b","c")
                        
                        tempMatrix[is.nan(tempMatrix)] <- 0
                        
                        exp.ylim = matrix(c(min(tempMatrix[,"a"]-0.05),max(tempMatrix[,"a"]+0.05),
                                            min(tempMatrix[,"b"] - 1),max(tempMatrix[,"b"] + 1),
                                            min(tempMatrix[,"c"] - 1),max(tempMatrix[,"c"] + 1)),
                                          byrow = TRUE,ncol = 2)
                }
                print(colnames(tempMatrix))
                for(k in 1:ncol(tempMatrix)){
                        print(exp.ylim[k, 2])
                        setEPS()
                        names = paste("./analysis-compare/code-tsp-trend/tsp_gompertModelpositive_size_",colnames(tempMatrix)[k],".eps",sep = "")
                        postscript(names)
                        x = c(1:length(instanceSize))
                        y = tempMatrix[,k]
                        #
                        plot(x, y, pch = 20, axes = FALSE, xlab = "", ylab = "", col = "red", ylim = exp.ylim[k, ], cex = 2, lwd = 2)
                       # axis(1,1:length(instanceSize),labels = instanceSize,las = 2, cex.lab = 1.1, cex.axis=1.1)
                        axis(1,1:length(instanceSize), labels = instanceSize, las = 2)
                        #text(x = c(1:length(instanceSize)), par("usr")[3] - 0.14*(exp.ylim[k, 2]-exp.ylim[k,1]), labels = instanceSize, srt = 60, xpd = TRUE, cex = 1.3)
                        axis(2, cex.lab = 1.3, cex.axis=1.5)
                        titleString = paste("Parameter ",toupper(colnames(tempMatrix)[k]),sep = "")
                        #titleString = paste(uni.algorithm,"_",toupper(colnames(tempMatrix)[k]),sep = "")
                        # itle(main = titleString, cex.main = 1.5 )
                        box()
                        grid(length(instanceSize)+2,col = "lightgray",lwd = 2)
                        dev.off()
                }
        }
        
     #   dev.off()
}

save.path = "./modelresults/tsp.analysing/"
# Analysis1(data.matrix, save.path = save.path )
# Analysis2(data.matrix, save.path = save.path )
Analysis3(data.matrix, save.path = save.path )
# 
# plot(x,y,pch = 3,axes = FALSE,xlab = "",ylab = "F",col = "red",ylim = pic.ylim[k,])
# axis(1,1:length(instanceSize),labels = FALSE)
# text(x = c(1:length(instanceSize)),par("usr")[3]-1.5,labels = instanceSize,srt = 45,xpd = TRUE)
# axis(2,labels = FALSE)
# text(y = y,par("usr")[1],labels = y,srt = 45,xpd = TRUE)