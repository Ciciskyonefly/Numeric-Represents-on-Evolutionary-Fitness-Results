rm(list = ls())
# source("./models_var.R")
# source("./models_func.R")
source("./base-funcs/readdata_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(dplyr)
library(data.table)
library(scmamp)
library(fBasics)

iter = 20
hiddensize = 6
maxiter = 50



file.path = "./modelresults/NN.maxsat.pre/maxTrain-maxTest/"
percent.filenames = list.files(file.path)

pattern <- paste( "residuals_" ,iter, "iter_", hiddensize, "hiddensize_", maxiter, "maxiter_",  sep = "")

list.names <-percent.filenames[grep(pattern, percent.filenames)]

per <- 2
all.residuals <- NULL
all.residuals.names <- NULL
for(per in 1:length(list.names)){
  #The path of the file
  str.string <-  do.call(rbind, strsplit(list.names[per],"_")) 
  all.residuals.names <- cbind(all.residuals.names, paste(str.string[1],"_",str.string[2], sep = ""))
  final.path = paste(file.path, list.names[per], sep = "")
  temp.data <- read.csv(file = final.path, header = TRUE, stringsAsFactors = FALSE)
  names(temp.data) <- c("instance_file", "residuals")
  write.csv(temp.data, file = final.path, row.names = FALSE )
  data.matrix <- LoadMaxsat(final.path)
  #  data.matrix <- data.matrix[-grep("experiment",data.matrix$instances %>% as.character ), ]
  
  temp.pre.data <- data.matrix
  if(which(is.na(temp.pre.data$residuals)) %>% length() != 0){
    temp.pre.data <- temp.pre.data[-which(is.na(temp.pre.data$residuals)), ]
  }
  
  split.resi <- split(temp.pre.data[c("instances", "algorithms", "residuals")], list(temp.pre.data$instances, temp.pre.data$algorithms), drop = TRUE)
  #  temp.aver.resi <- aggregate(cbind(a, b, c, d, residuals)~ instances +algorithms, data = temp.pre.data, FUN = mean)
  
  stat.Mean <- NULL
  stat.Median <- NULL
  stat.SE_Mean <- NULL
  stat.Variance <- NULL
  stat.Stdev <- NULL
  
  split <- 1
  for(split in 1:length(split.resi)){
    print(split)
    base.stat <- split.resi[[split]]$residuals %>% basicStats()
    
    stat.Median <- rbind(stat.Median, base.stat["Median", ])
    stat.Mean <- rbind(stat.Mean, base.stat["Mean", ])
    stat.SE_Mean <- rbind(stat.SE_Mean, base.stat["SE Mean", ])
    stat.Variance <- rbind(stat.Variance, base.stat["Variance", ])
    stat.Stdev <- rbind(stat.Stdev, base.stat["Stdev", ])
    
  }
  
  rownames(stat.Mean) <- names(split.resi)
  rownames(stat.Median) <- names(split.resi)
  rownames(stat.SE_Mean) <- names(split.resi)
  rownames(stat.Variance) <- names(split.resi)
  rownames(stat.Stdev) <- names(split.resi)
  
  if(is.null(all.residuals)){
    all.residuals <- cbind(all.residuals, rownames(stat.Median))
  }
  all.residuals <- cbind(all.residuals, stat.Median[, 1])
}  

all.residuals <- all.residuals[, -1]         
colnames(all.residuals) <- all.residuals.names

 savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_NN_maxTrain_maxTest_single_run.csv" , sep = "")
 write.csv(all.residuals,file =  savecsv.names, row.names = TRUE)
 
cex.save <- all.residuals[, which(colnames(all.residuals) %in% c("50_100", "10_100", "100_1000"))]
cex.save <- cex.save[, c(3, 1, 2)]
caption  <- paste("Maxsat NN Residuals in Different Data Percentage", sep = "")
savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_NN_maxTrain_maxTest_single_run.tex" , sep = "")

writeTabular(cex.save[c(41:50), ], savetex.names, caption = caption,
             table.position = "htbp", align = "c",
             digits = 3,
             wrap.as.table = TRUE,
             centering = TRUE,
             caption.position = "t"
)


