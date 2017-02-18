#compare 


rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(dplyr)
library(data.table)
library(scmamp)
library(fBasics)

list_model <- list(logisticModelpositive,
                   decayModelpositive,
                   decayModelnegative,
                   expLinearModelpositive
)

size = 10
file.path <- "./modelresults/LM.maxsat.pre/"
percent.filenames <- list.files(file.path)
stat<- NULL
save.path <- "./analysis-compare/statisticalproperty/"

for(mod in 1:length(list_model)){
  
  save.residuals <- NULL
  col.names <- c("instance_file")
  
  for(per in 1:length(percent.filenames)){
    
    data.path <- paste(file.path, percent.filenames[per], "/",sep = "") %>% list.files()
    
    pattern <- paste(size, ".+", unlist(list_model[mod])$name,".+.csv$", sep = "")
    data.path <- data.path[grep(pattern, data.path)]
    final.path = paste(file.path, percent.filenames[per], "/", data.path, sep = "")
    #  print(per)
    temp.data <- read.csv(file = final.path, header = TRUE, stringsAsFactors = FALSE)
    if(per == 1){
      save.residuals =  cbind(save.residuals, temp.data$instance_file %>% as.vector() )
    }
    
    save.residuals <- cbind(save.residuals, round(temp.data$residuals, digits = 4))
    
    col.names <- col.names %>% cbind(percent.filenames[per])
    
  }
  
  colnames(save.residuals) <- col.names
  
  
  data.matrix <- save.residuals %>% data.frame()
  data.matrix <- data.matrix[-grep("experiment.xml", data.matrix$instance_file), ]
  
  uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms.maxsat()
  uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 2] 
  instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
  data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
  data.matrix$instances <- do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% factor(levels = instance.size, ordered = TRUE)
  
  save.residuals <- data.matrix
  savetex.names = paste("../paper-latex/Rtex/maxsat_",unlist(list_model[mod])$name,"_prediction_residuals.tex" , sep = "")
  print(head(save.residuals))
  
  
  split.resi <- split(save.residuals[, c(3:11, 2)], list(save.residuals$instances, save.residuals$algorithms)) 
  
  stat.Mean <- NULL
  stat.Median <- NULL
  stat.SE_Mean <- NULL
  stat.Variance <- NULL
  stat.Stdev <- NULL
  
  for(i in 1:length(split.resi)){
    base.stat <- split.resi[[i]] %>% unfactor %>% data.matrix %>% basicStats()
    
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
  colnames(stat.Stdev) <- c("$10\\%$", "$20\\%$", "$30\\%$", "$40\\%$", "$50\\%$",
                           "$60\\%$", "$70\\%$", "$80\\%$", "$90\\%$", "$100\\%$")
  #TODO ONLY GENERATE ONE CSV        
  savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_",unlist(list_model[mod])$name,"_stat_Stdev.csv" , sep = "")
  write.csv(stat.Stdev,file =  savecsv.names, row.names = TRUE)
  caption  <- paste("Maxsat_", unlist(list_model[mod])$name," Residuals in Different Data Percentage", sep = "")
  savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_",unlist(list_model[mod])$name,"_stat_Stdev.tex" , sep = "")
  
  writeTabular(round(stat.Stdev[c(1:10, 31:40), ], 3), savetex.names, caption = caption,
               table.position = "htbp", align = "c",
               wrap.as.table = TRUE,
               centering = TRUE,
               caption.position = "t"
  )
  
  rownames(stat.Mean) <- names(split.resi)
  colnames(stat.Mean) <- c("$10\\%$", "$20\\%$", "$30\\%$", "$40\\%$", "$50\\%$",
                            "$60\\%$", "$70\\%$", "$80\\%$", "$90\\%$", "$100\\%$")
  savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_",unlist(list_model[mod])$name,"_stat_Mean.csv" , sep = "")
  write.csv(stat.Mean,file =  savecsv.names, row.names = TRUE)
  caption  <- paste("Maxsat_", unlist(list_model[mod])$name," Residuals in Different Data Percentage", sep = "")
  savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/maxsat_",unlist(list_model[mod])$name,"_stat_Mean.tex" , sep = "")
  
  writeTabular(round(stat.Mean[c(1:10, 31:40), ], 3), savetex.names, caption = caption,
               table.position = "htbp", align = "c",
               wrap.as.table = TRUE,
               centering = TRUE,
               caption.position = "t"
  )
}

# csv.names <- names(stat)
# for(i in 1:length(stat)){
#   data <- stat[i] %>% data.frame()
#   
#   save.file = paste(save.path, "LM_maxsat_", csv.names[i], ".csv", sep = "")
#   cat(save.file,"\n")
#   write.csv(data, file = save.file, row.names = FALSE)
#   
#   
#   
# }




