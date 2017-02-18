rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(dplyr)
library(data.table)
library(scmamp)
library(fBasics)
list.model <- list(logisticModelpositive)

rawdata.path <- "./rawdata/maxsat/"

library(dplyr)
file.path <- "./modelresults/LM.maxsat.pre/"
percent.filenames <- list.files(file.path)
percent.filenames <- percent.filenames[c(2:10, 1)]
volume <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
size <- 10
all.residuals <- NULL
i <- 1
mod <- 1
for(i in 1:length(volume)){
  data.path <- paste(file.path, percent.filenames[i],"/", sep = "") 
  list.data.path <- data.path%>%list.files()
  pattern <- paste(size, ".+", unlist(list.model[mod])$name,".+csv$", sep = "")
  file.name <- list.data.path[grep(pattern, data.path %>% list.files())]
  final.path <- paste(data.path, file.name, sep = "")
  
  data.matrix <- LoadMaxsat(final.path)
  data.matrix <- data.matrix[-grep("experiment",data.matrix$instances %>% as.character ), ]
  temp.pre.data <- data.matrix
  temp.pre.data %>% nrow()
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
  
  
  
  
  ###average model way
  #data.matrix <- read.csv(final.path)
  # data.matrix <- data.matrix[-grep("experiment",data.matrix$instance_file %>% as.character ), ]
  # uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms.maxsat()
  # uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2]
  # #which instances.
  # runs <- do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/|-"))[, 3]
  # 
  # instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
  # data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
  # data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)
  # data.matrix$runs <- runs %>% factor
  # temp.pre.data <- data.matrix
  # temp.pre.data %>% nrow()
  # if(which(is.na(temp.pre.data$residuals)) %>% length() != 0){
  #   temp.pre.data <- temp.pre.data[-which(is.na(temp.pre.data$residuals)), ]
  # }
  # 
  # model.aver.para <- aggregate(cbind(a, b, c, d, residuals)~ runs +instances +algorithms, data = temp.pre.data, FUN = mean)
  # model.aver.para %>% nrow()
  # dat <- 1
  # pre.predict_error <- NULL
  # for(dat in 1:nrow(model.aver.para)){
  #   
  #   pattern <- paste( ".+/",model.aver.para$algorithms[dat],"_", model.aver.para$instances[dat], "-", model.aver.para$runs[dat] , sep = "")
  #   final.file <- paste(rawdata.path,rawdata.path %>% list.files(), sep = "")
  #   raw.data <- final.file[grep(pattern, final.file)] %>% read.csv
  #   index <- seq(1: ceiling(nrow(raw.data) * volume[i]))
  #   volume.raw.data <- raw.data[-index, ]
  #   pre.predict_y <- list.model[[mod]]$modelFunction( model.aver.para[dat, c("a", "b", "c", "d") ] %>% t() %>% as.numeric(), xData = volume.raw.data$x)
  #   pre.predict_error <- rbind(pre.predict_error, xyRMSE(volume.raw.data$y, pre.predict_y) )
  # }
  # #print(pre.predict_error)
  # model.aver.para$residuals <- pre.predict_error
  # cat("volume", volume[i], "-----------------------------\n")
  # temp.aver.resi <- aggregate(cbind(residuals) ~ instances + algorithms, data = model.aver.para, FUN = mean)
  # 
 if(is.null(all.residuals)){
   all.residuals <- cbind(all.residuals, rownames(stat.Mean))
 }
  all.residuals <- cbind(all.residuals, stat.Median[, 1])
}




print(all.residuals)
all.residuals <- all.residuals[, -1] %>% data.frame()
colnames(all.residuals) <- c("$10\\%$", "$20\\%$", "$30\\%$", "$40\\%$", "$50\\%$",
                          "$60\\%$", "$70\\%$", "$80\\%$", "$90\\%$",  "$100\\%$")
#TO


write.csv(all.residuals, file = "./analysis-compare/res-h_percents_error_in_prediction/maxsat_logisticModelpositive_stat_Median_sing_run.csv", row.names = TRUE)
caption  <- paste("Maxsat_", unlist(list.model[mod])$name," Residuals in Different Data Percentage", sep = "")
savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_",unlist(list.model[mod])$name,"_stat_Median_single_run.tex" , sep = "")
writeTabular(all.residuals[c(1:10, 31:40), ], savetex.names, caption = caption,
             table.position = "htbp", align = "c",
             wrap.as.table = TRUE,
             digits = 3,
             centering = TRUE,
             caption.position = "t"
)
