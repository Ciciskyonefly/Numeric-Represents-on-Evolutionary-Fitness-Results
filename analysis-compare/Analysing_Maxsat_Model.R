rm(list = ls())
library("BB")
library("ggplot2")
#include RMSE
#library(qpcR)
library(Formula)
#library(scatterplot3d)
library(dplyr)

source('./base-funcs/models_var.R')

list_model <- list(logisticModelpositive,logisticModelnegative,
                   decayModelpositive,decayModelnegative,
                   gompertzModelpositive,gompertzModelnegative,
                   expLinearModelpositive,expLinearModelnegative)



OrderAlgorithms <- function(uni.alg){
        # Arrange algorithms by algorithms stratagy
        # 
        # Args:
        #   uni.alg: A vector or a list of algorithms need to be ordered.
        #
        # Return:
        #   A vector or list of algorithms.
        
        uni.alg <- unique(uni.alg) %>% sort()
        uni.alg.Crs = uni.alg[grep("Crs", uni.alg)]
        uni.alg.noCrs = uni.alg[-grep("Crs", uni.alg)]
        uni.alg = c(uni.alg.noCrs, uni.alg.Crs)
        
        return (uni.alg)
}



load<-function(){
        # Load Data
        #
        #
        #
        
        data.table = read.csv("./modelresults/LM.maxsat.pre/100percentleft/10_logisticModelpositive_model.csv",
                              header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        
        if(length(grep("experiment", data.table[, 1])) != 0){
                data.table =  data.table[-grep("experiment", data.table[, 1]), ]
        }
        if(length(which(data.table[,"residuals"] == 1000000)) > 0)
                data.table = data.table[-which(data.table[,"residuals"] == 1000000),]
        #data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        
        return(data.table)
}

maxsat_file_path = "./BenchMarking/modelresults/LM.maxsat.pre/100percentleft/"
maxsat_save_path = "./BenchMarking/modelresults/maxsat.result.trend/"

file.path = maxsat_file_path
save.path = maxsat_save_path
size = 10
# PlotValuesofParametersTrend <- function(file.path = "./BenchMarking/modelresults/LM.maxsat/", save.path = "./BenchMarking/modelresults/maxsat.result.trend/",size = 10){
#         
#         
#         
#         list.name = list.files(file.path)
#         csv.list= grep(".csv",list.name)
#         if(length(csv.list) != 0){
#                 list.name = list.name[csv.list] 
#         }else{
#                 cat("no useful data.")
#                 break
#         }
#         
#         size = paste(size, "_", sep = "")
#         size.list = grep(size,list.name)
#         if(length(size.list)==0){
#                 cat("no useful data.")
#                 break
#         }else
#                 list.name = list.name[size.list]
# 
#         pdfname = "./modelresults/maxsat.analysing/logistic_1.pdf"
#         pdf(pdfname)
#         par(mfrow = c(2,2))
#         
#         for( pp in 1:length(list.name)){
#                 
#                 data.matrix <- read.csv(paste(file.path,list.name[pp],sep = ""))
#                 
#                 
#                 uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms()
#                 uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2] 
#                 instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
#                 
#                 data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
#                 data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)
#                 
#                 # Get average mean by instance and algorithm
#                 mean.data <- group_by(data.matrix,instances,algorithms) %>% summarise(mean(a),mean(b),mean(c),mean(d)) %>% data.frame()
#                 mean.data <- mean.data[, c(3:6,1,2)]
#                 qipa <- split(mean.data, mean.data$algorithms, drop = TRUE)
#                 tempnames <- c("a", "b", "c", "d")
#                 for( i in 1:4){
#                         
#                         ylim = c(min(mean.data[, i]), max(mean.data[, i])) 
#                         legend.names <- NULL
#                         for( j in 1:length(qipa)){
#                                 
#                                 legend.names <- cbind(legend.names, names(qipa[j]))
#                                 temp_qipa = qipa[j] %>% data.frame()
#                                 instances.labels <-  dplyr::select(temp_qipa, ends_with("instances"))[, 1] %>% as.vector()
#                                 a_b_c_d <-  dplyr::select(temp_qipa,matches("mean."))
#                                 
#                                 
#                                 if(j != 1) {
#                                         lines(c(1:10), a_b_c_d[, i] %>% as.vector(), col = j, type = "b", lwd = 1, pch = 6+j) 
#                                 } else {
#                                         
#                                         a_b_c_d[, i] %>% as.vector() %>% plot(axes = FALSE,type = "b", pch = 6+j, lwd = 1, xlab = "", ylim = ylim)
#                                         axis(1,1:10,instances.labels)
#                                         axis(2)
#                                         main.string = paste(unlist(strsplit(list.name[pp],"_"))[2], "_", tempnames[i], sep = "")
#                                         title(main = main.string)
#                                         grid()
#                                         box()
#                                         
#                                 }
#                                 
#                         }
#                         
#                         if(i - 2.1 > 0){
#                                 legend("topright", lwd = 1, pch = c(7:(6+length(qipa))), legend = legend.names, col = c(1:length(qipa)), cex = 0.45)
#                         } else {
#                                 legend("topleft", lwd = 1, pch = c(7:(6+length(qipa))), legend = legend.names, col = c(1:length(qipa)),  cex = 0.45)
#                                 
#                         }
#                 }     
#                 
#         }
#         
#         dev.off()
# }
# 
#PlotValuesofParametersTrend()
# pdfname = "./BenchMarking/modelresults/maxsat.analysing/collect_logistic_1.pdf"
# pdf(pdfname)
#par(mfrow = c(2,2))




# Get average mean by instance and algorithm
data.matrix <-load()
uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms()
uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2] 
instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()

data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)


mean.data <- group_by(data.matrix,instances,algorithms) %>% summarise(mean(a),mean(b),mean(c),mean(d)) %>% data.frame()
mean.data <- mean.data[, c(3:6,1,2)]
names(mean.data) <- c("mean_a", "mean_b", "mean_c", "mean_d", "instances", "algorithms")
qipa <- split(mean.data, mean.data$algorithms, drop = TRUE)

for( i in 1:4){
        
        setEPS()
        names = paste("./proposal-paper/logistic_summary_",names(mean.data)[i],".eps",sep = "")
        postscript(names)
        
        ylim = c(min(mean.data[, i]), max(mean.data[, i])) 
        legend.names <- NULL
        for( j in 1:length(qipa)){
                
                legend.names <- cbind(legend.names, names(qipa[j]))
                temp_qipa = qipa[j] %>% data.frame()
                instances.labels <-  dplyr::select(temp_qipa, ends_with("instances"))[, 1] %>% as.vector()
                a_b_c_d <-  dplyr::select(temp_qipa,matches("mean."))
                names(a_b_c_d) <- c("Parameter A", "Parameter B", "Parameter C", "Parameter D")
               
                if(j != 1) {
                        lines(c(1:10), a_b_c_d[, i] %>% as.vector(), cex = 1.5, col =1 + ifelse(j>3, j-3, j), lty = ifelse(j>3, 6,1), type = "b", lwd = 3, pch = 15+ifelse(j>3, j-3, j)) 
                } else {
                        
                        a_b_c_d[, i] %>% as.vector() %>% plot(axes = FALSE,  cex = 1.5, type = "b",lty = ifelse(j>3, 2, 1), col = 1+ ifelse(j>3, j-3, j), pch = 15+ifelse(j>3, j-3, j), lwd = 3, xlab = "", ylim = ylim)
                        axis(1,1:10,instances.labels)
                        axis(2)
                     #   title(main = names(a_b_c_d)[i])
                     #   grid()
                        box()
                        
                }
                
        }
        
       if(i - 2.1 > 0){
               legend("topright", lwd = 2, ncol = 2, lty = c(1, 1, 1, 6, 6, 6), pch = c(16: (15+length(qipa)/2)), legend = legend.names, col = c(2:(length(qipa)/2 + 1)),  cex = 1.2)
       } else {
               legend("topleft", lwd = 2,  ncol = 2, lty = c(1, 1, 1, 6, 6, 6), pch = c(16: (15+length(qipa)/2)), legend = legend.names,  col = c(2:(length(qipa)/2 + 1)),  cex = 1.2)
               
       }
        
        dev.off()
}     







