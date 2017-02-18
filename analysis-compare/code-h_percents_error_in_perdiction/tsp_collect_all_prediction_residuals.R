rm(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(dplyr)
library(data.table)
library(scmamp)
library(varhandle)

list_model <- list(gompertzModelpositive)


file.path = "./modelresults/LM.tsp.pre/"
percent.filenames = list.files(file.path)

save_path = "./modelresults/tsp.analysing/"

for(mod in 1:length(list_model)){
        
        save.residuals <- NULL
        col.names <- c("instance_file")
        
        for(per in 1:length(percent.filenames)){
                
                data.path <- paste(file.path, percent.filenames[per], "/",sep = "") %>% list.files()
                
                pattern <- paste("10_", unlist(list_model[mod])$name,".+.csv$", sep = "")
                
                
                data.path <- data.path[grep(pattern, data.path)]
                
                
                final.path = paste(file.path, percent.filenames[per], "/", data.path, sep = "")
                
                temp.data <- read.csv(file = final.path, header = TRUE, stringsAsFactors = FALSE)
                
                if(per == 1){
                        save.residuals =  cbind(save.residuals, temp.data$instance_file %>% as.vector() )
                }
                
                save.residuals <- cbind(save.residuals, temp.data$residuals)
                
                col.names <- col.names %>% cbind(percent.filenames[per])
                
        }
        
        colnames(save.residuals) <- col.names
        colnames(save.residuals)[1] <- "instance_file"
        
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
        
        
        print(head(stat.Mean))
        #TODO ONLY GENERATE ONE CSV        
        
        colnames(stat.Mean) <- c("$10\\%$", "$20\\%$", "$30\\%$", "$40\\%$", "$50\\%$",
                                 "$60\\%$", "$70\\%$", "$80\\%$", "$90\\%$", "$100\\%$")
        savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/tsp_",unlist(list_model[mod])$name,"_stat_Mean.csv" , sep = "")
        write.csv(stat.Mean,file =  savecsv.names, row.names = TRUE)
        caption  <- paste("TSP", unlist(list_model[mod])$name," Residuals in Different Data Percentage", sep = "")
        savetex.names = paste("./analysis-compare/res-h_percents_error_in_prediction/tsp_",unlist(list_model[mod])$name,"_stat_Mean.tex" , sep = "")
        
        writeTabular(round(stat.Mean[grep("ts10r10f", rownames(stat.Mean)), ], 3), savetex.names, caption = caption,
                     table.position = "htbp", align = "c",
                     wrap.as.table = TRUE,
                     centering = TRUE,
                     caption.position = "t"
        )
}


data.frame(save.residuals)

