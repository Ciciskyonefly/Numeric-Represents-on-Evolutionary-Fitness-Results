rm(list = ls())
# Plot six algorithms runtime behavior in one instances.

source("./models_var.R")
source("./models_func.R")
source("./data_preprocessing/readdata.R")
source("./compare_method_1/maxsattsp_load_and_order.R")
library(dplyr)
library(data.table)


list_model <- list(logisticModelpositive,
                   decayModelpositive,decayModelnegative,
                   expLinearModelpositive)

OneInstancesInSixAlgorithms <- function(file.path,size = 10){
        # Plot six algorithms runtime behavior in one instances.
        
        
        
        list.name = list.files(file.path)
        csv.list = grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }
        
        for(mod in 1:length(list_model)){
                
                
                subscript = unlist(list_model[mod])$name %>% grep(list.name)
                if(length(subscript) == 0)
                        cat(unlist(list_model[mod])$name," data not exists . \n")
                
                final.path = paste(file.path, list.name[subscript],sep = "")
                
                data.matrix <- LoadMaxsat(final.path)
                model <- eval(parse(text = as.vector(data.matrix$model[1])))
                mean.data <- group_by(data.matrix,instances,algorithms) %>% summarise(mean(a),mean(b),mean(c),mean(d)) %>% data.frame()
                mean.data <- mean.data[, c(3:6,1,2)]
                split.data <-  split(mean.data, mean.data$instances)
                
                # pdfname = paste("./compare_method_1/", unlist(list_model[mod])$name, "_one_instances_in_six_algorithms.pdf" , sep = "")
                # pdf(pdfname)
                # par(mfrow = c(2, 2))
                for(ins_i in 1:length(split.data)){
                        
                        setEPS()
                        names = paste("./tempeps/", names(split.data[ins_i]), "_", unlist(list_model[mod])$name, "_maxsat_one_in_six_algorithms.eps", sep = "") # pic name
                        postscript(names)
                        
                        test.data <- split.data[ins_i] %>% data.frame()
                        parameters <- select(test.data, c(1:4)) %>% data.matrix
                        innerpath = paste(select(test.data, ends_with("algorithms"))[ ,1] %>% as.vector(),"/",select(test.data, ends_with("instances"))[ ,1] %>% as.vector(),sep = "")
                        
                        legend.names <- select(test.data, ends_with("algorithms"))[, 1] %>% as.vector()
                        
                        title <- select(test.data, ends_with("instances"))[, 1] %>% as.vector() %>% unique()
                        
                          Total_path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/maxSat/results/"
                     
                         
                        file.data <- NULL
                        for(i in 1:length(innerpath)){
                                algorithm.list <- paste(Total_path, legend.names[i],"/", sep = "")
                                instances.list <- list.files(algorithm.list) 
                                respath <- paste(algorithm.list, "/", instances.list[grep(title, instances.list)], "/", sep = "") 
                                for(j in 1:length(respath))
                                        file.data <- getMatrixData(respath[j]) %>% rbind(file.data)
                        }
                        
                        
                        x <- seq(0.1, 100000, by = 0.2)
                        ylim = c(min(file.data[, 3]), max(file.data[, 3]))
                        for(i in 1:nrow(parameters)){
                                if(i == 1){
                                        GetFormulaValue(model$formula, x, parameters[i, ] %>% as.vector()) %>% plot(log = "x", 
                                                                                                                    xlab = "", ylab = "",
                                                                                                                    col = i, lwd = 2,
                                                                                                                    ylim = ylim,
                                                                                                                    type = "l") 
                                } else {
                                        GetFormulaValue(model$formula, x, parameters[i, ] %>% as.vector()) %>% lines( col = i, lwd =2) 
                                }
                                
                        }
                        
                        title(main = title)
                        legend("topright", legend = legend.names, lwd = 2, col = c(1:nrow(parameters)),  cex = 0.7)
                        grid(lwd = 1)
                        
                        dev.off()
                        
                }
                
                #dev.off()
        }
        
        
        
        
}

file.path = "./modelresults/LM.maxsat.pre/100percentleft/"
OneInstancesInSixAlgorithms(file.path)
size = 10
