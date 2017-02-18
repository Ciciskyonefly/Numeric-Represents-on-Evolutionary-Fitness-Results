rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(dplyr)
library(data.table)
library(scmamp)
library(fBasics)
library(varhandle)
iter = 20
hiddensize = 6
maxiter = 150



file.path = "./modelresults/NN.tsp.pre/maxTrain-maxTest/"
percent.filenames = list.files(file.path)

save_path = "./modelresults/NN.analysing/"


save.residuals <- NULL
col.names <- c("instance_file")

for(per in 1:length(percent.filenames)){
        
        data.path <- paste(file.path, percent.filenames[per], "/",sep = "") %>% list.files()
        pattern <- paste( "residuals_" ,iter, "iter_", hiddensize, "hiddensize_", maxiter, "maxiter_",  sep = "")
        
        if(0 == grep(pattern, data.path) %>% length())
                next
        data.path <- data.path[grep(pattern, data.path)]
        
        #The path of the file
        final.path = paste(file.path, percent.filenames[per], "/", data.path, sep = "")
        temp.data <- read.csv(file = final.path, header = TRUE)
        names(temp.data) <- c("instance_file", "residuals")
        # print(temp.data)
        
        if(is.null(save.residuals )){
                save.residuals =  cbind(save.residuals, temp.data$instance_file %>% as.vector() )
        }
        
        save.residuals <- cbind(save.residuals, temp.data$residuals)
        col.names <- col.names %>% cbind(percent.filenames[per])
        # print(save.residuals)
}

colnames(save.residuals) <- col.names


data.matrix <- save.residuals %>% data.frame()
uni.algorithm = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% unique() %>% OrderAlgorithms.tsp()
uni.datafile = do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 3] %>% OrderInstance.tsp()
instance.size = do.call(rbind, strsplit(uni.datafile,"-"))[, 1] %>% unique()
data.matrix$algorithms <-  do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 1] %>% factor(levels = uni.algorithm, ordered = TRUE)
data.matrix$instances <- do.call(rbind, strsplit(as.vector(data.matrix$instance_file),"/"))[, 3] %>% factor(levels = uni.datafile, ordered = TRUE)

save.residuals <- data.matrix
print(save.residuals)

split.resi <- split(save.residuals[, c(3:11, 2)], list(save.residuals$instances, save.residuals$algorithms)) 

stat.Mean <- NULL
stat.Median <- NULL
stat.SE_Mean <- NULL
stat.Variance <- NULL
stat.Stdev <- NULL

for(i in 1:length(split.resi)){
  base.stat <- split.resi[[i]] %>% unfactor() %>% data.matrix() %>% basicStats()
  #    print(base.stat)
  stat.Median <- rbind(stat.Median, base.stat["Median", ])
  stat.Mean <- rbind(stat.Mean, base.stat["Mean", ])
  stat.SE_Mean <- rbind(stat.SE_Mean, base.stat["SE Mean", ])
  stat.Variance <- rbind(stat.Variance, base.stat["Variance", ])
  stat.Stdev <- rbind(stat.Stdev, base.stat["Stdev", ])
  
}

head(stat.Mean)

rownames(stat.Mean) <- names(split.resi)
rownames(stat.Median) <- names(split.resi)
rownames(stat.SE_Mean) <- names(split.resi)
rownames(stat.Variance) <- names(split.resi)
rownames(stat.Stdev) <- names(split.resi)

colnames(stat.Mean) <- c("$10\\%$", "$20\\%$", "$30\\%$", "$40\\%$", "$50\\%$",
                          "$60\\%$", "$70\\%$", "$80\\%$", "$90\\%$", "$100\\%$")
savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/tsp_NN_stat_Mean.csv" , sep = "")
write.csv(stat.Mean,file =  savecsv.names, row.names = TRUE)
caption  <- paste("TSP NN Residuals in Different Data Percentage", sep = "")
savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/tsp_NN_stat_Mean.tex" , sep = "")

writeTabular(round(stat.Mean[grep("ts10r10f", rownames(stat.Mean)), ], 3), savetex.names, caption = caption,
             table.position = "htbp", align = "c",
             wrap.as.table = TRUE,
             centering = TRUE,
             caption.position = "t"
)

# 
# 
# 
# 
# writeTabular(save.residuals,"../paper-latex/Rtex/tsp_NN_prediction_collect_residuals.tex" )
# save.residuals %>% write.csv(file = save.names, row.names = FALSE)
# 
# 
