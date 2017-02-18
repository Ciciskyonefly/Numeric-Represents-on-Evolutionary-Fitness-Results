rm(list = ls())
source("./models_var.R")
source("./models_func.R")
source("./data_preprocessing/readdata.R")
source("./compare_method_1/maxsattsp_load_and_order.R")
library(dplyr)
library(data.table)


data.matrix <- LoadTsp()
model <- eval(parse(text = as.vector(data.matrix$model[1])))
mean.data <- group_by(data.matrix,instances,algorithms) %>% summarise(mean(a),mean(b),mean(c),mean(d)) %>% data.frame()
mean.data <- mean.data[, c(3:6,1,2)]
split.data <-  split(mean.data, mean.data$instances)

# pdfname = "./compare_method_1/tsp_one_instances_in_six_algorithms.pdf"
# pdf(pdfname)
# par(mfrow = c(2, 2))





for(ins_i in 1:length(split.data)){
        
        setEPS()
        names = paste(names(split.data[ins_i]), "_", "tsp_one_in_six_algorithms.eps", sep = "") # pic name
        postscript(names)
        
        test.data <- split.data[ins_i] %>% data.frame()
        parameters <- select(test.data, c(1:4)) %>% data.matrix
        innerpath = paste(select(test.data, ends_with("algorithms"))[ ,1] %>% as.vector(),"/symmetric/",select(test.data, ends_with("instances"))[ ,1] %>% as.vector(),sep = "")
        
        legend.names <- select(test.data, ends_with("algorithms"))[, 1] %>% as.vector()
        
        title <- select(test.data, ends_with("instances"))[, 1] %>% as.vector() %>% unique()
        
        
        Total_path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"
        
        file.data <- NULL
        
        for(i in 1:length(innerpath)){
                        file.path <- paste(Total_path, innerpath[i], "/", sep = "")
                        file.data <- getTheMatrixData_TspSuite(file.path) %>% rbind(file.data)
        }
        
        
        x <- seq(0.1, 10000, by = 0.1)
        ylim = c(min(file.data[, 3]), max(file.data[, 3]))
        for(i in 1:nrow(parameters)){
                if(i == 1){
                        GetFormulaValue(model$formula, x, parameters[i, ] %>% as.vector()) %>% plot(log = "x", 
                                                                                                    xlab = "", ylab = "",
                                                                                                    col = i, lwd = 2,
                                                                                                    ylim = ylim,
                                                                                                    type = "l") 
                } else {
                        GetFormulaValue(model$formula, x, parameters[i, ] %>% as.vector()) %>% lines( col = i, lwd = 2) 
                }
                
        }
        
        title(main = title)
        legend("topright", legend = legend.names, lwd = 2, col = c(1:nrow(parameters)),  cex = 0.7)
        grid(lwd = 1)
        
        dev.off()
        
}

#dev.off()