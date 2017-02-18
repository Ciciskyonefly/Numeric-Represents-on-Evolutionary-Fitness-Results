rm(list = ls())
library(caret)
library(dplyr)
library(nnet)
library(ggplot2)
library(stringr)
library(data.table)
library(varhandle)
library(scmamp)
set.seed(3456)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")

list.model <- list(gompertzModelpositive)
loc.path = "./modelresults/LM.tsp.pre/100percentleft/"
all.path <- loc.path %>% list.files()

#prediction.path <- "./analysis-compare/tsp_parameters_prediction/"
#prediction.file <- prediction.path %>% list.files()

allresiduals <- NULL
mod = 1

for(mod in 1:length(list.model)){
        
        data.prediction <- LoadTsp("./analysis-compare/tsp_parameters_prediction/tsp_predict_parameters.csv")
        data.prediction <- data.prediction[order(data.prediction$instances %>% as.character), ]
        data.prediction <- data.prediction[order(data.prediction$algorithms %>% as.character), ] 
        data.prediction$size <- str_extract_all(data.prediction$instances, "[0-9]+")
        parameters.dat <- dplyr::select(data.prediction, a, b, c, d)
        
        path = "./rawdata/tsp/"
        csvnames <- path %>% list.files()
        predict.resdiuals <- NULL
        list_plot = list()
        
        pdfname <- paste("./analysis-compare/tsp_parameters_prediction/", list.model[[mod]]$name, "_predict_parameter_curve.pdf", sep = "")
        count <- 0
        model.pattern <- paste("10_", list.model[[mod]]$name, ".+.csv", sep = "")
        file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
        model.data <- read.csv(file.path)
        
        dat.i = 1
        for(dat.i in 1: nrow(parameters.dat)){
                
                predict_error <- 0
                pattern <- paste(data.prediction$algorithms[dat.i],"_.+", data.prediction$size[dat.i], sep = "")
                
                temp.listnames <- csvnames[grep(pattern, csvnames)]
                each <- 1
                for(each in 1:length(temp.listnames)){
                        
                        data.path = paste(path, temp.listnames[each], sep = "")
                        print(data.path)
                        raw.data = read.csv(data.path, header = TRUE)
                        predict_y <- list.model[[mod]]$modelFunction(parameters.dat[dat.i, ] %>% t() %>% as.numeric(), xData = raw.data$x)
                        pic = list.model[[mod]]$plotFunction(raw.data, parameters.dat[dat.i, ] %>% t() %>% as.numeric())
                        temp.name <- strsplit(temp.listnames[each], "\\.") %>% unlist()
                        pic.names <- paste( list.model[[mod]]$name, temp.name[1])
                        pic <- pic + labs(title = pic.names) + theme(plot.title = element_text(hjust = 0.5))
                        
                        
                        
                        model.parameters <- model.data[grepl(gsub("_", "/", temp.name[1]), model.data$instance_file), ] %>% dplyr::select(a,b,c,d) %>% t() %>% as.numeric()
                        model_y <- list.model[[mod]]$modelFunction(model.parameters, xData = raw.data$x)
                        model_X <- raw.data$x
                        model.line <- data.frame(model_X, model_y)
                        pic <- pic + geom_line(mapping = aes(model_X, model_y), data = model.line)
                        
                        
                        predict_error <- xyRMSE(raw.data$y, predict_y) + predict_error
                        count <- count + 1
                        list_plot[[count]] <- pic
                }
                
                aver_error <- predict_error/(length(temp.listnames))
                
                predict.resdiuals <- predict.resdiuals %>% rbind(round(aver_error, 5)) 
                
        }
        
        start.time <- Sys.time()
        library(gridExtra)
        ggsave(filename = pdfname,  marrangeGrob(grobs = list_plot, nrow = 2, ncol = 2))
        end.time <- Sys.time()
        print(end.time - start.time)
        predict.res <- data.frame(algorithms = data.prediction$algorithms, size = data.prediction$size %>% as.character(), 
                                  residuals = predict.resdiuals)

        #generate table
        predict.res <- predict.res[order(predict.res$size), ]
        predict.res <- aggregate(predict.res$residuals, predict.res[, c(1, 2)], mean)
        colnames(predict.res)[3] <- "predict.residuals" 
        
        residuals <- model.data[, "residuals"]
        alldata <- LoadTsp(file.path)
        residual.data <-  dplyr::select(alldata, algorithms, instances,  residuals)
        residual.data$size <- str_extract(residual.data$instances, "[0-9]+") %>% as.numeric() %>% factor()
        res <- aggregate( residual.data$residuals, residual.data[, c(1,4)], mean)
        res$algorithms
        res <- res[order(res$algorithms), ]
        colnames(res) <- c("algorithms", "size", "resdiuals")
        
        exp.sup <- grep("experiment", res$size)
        if(exp.sup %>% length() !=0 ){
                res <- res[-exp.sup, ]
        }
        
        predict.res$residuals <- res$resdiuals
        
        pp <- split(predict.res, predict.res$algorithms)
        
        pdfname <- paste("./analysis-compare/tsp_parameters_prediction/", list.model[[mod]], "_predict_parameters_residuals.pdf", sep = "")
        pdf(pdfname)
        
        for(i in 1:length(pp)){
                
                plot(1:nrow(pp[[i]]), pp[[i]]$predict.resdiuals, 
                     type = "b",
                     col = "red", pch = 3, size = 2,
                     ylim = c(0, 5), axes = FALSE,
                     xlab = "Instances size",
                     ylab = "Residuals")
                lines(1:nrow(pp[[i]]), pp[[i]]$residuals, type = "b")
                axis(1, 1:nrow(pp[[i]]), labels = pp[[i]]$size)
                axis(2)
                box()
                grid()
                title(main = names(pp)[i])
                legend("topleft", legend = c("Predict", "No-Predict"))
        }
        
        
        
        if(mod == 1){
                allresiduals <- allresiduals %>% cbind(predict.res$algorithms %>% as.character, predict.res$size)
        }
        
        allresiduals <- allresiduals %>% cbind(predict.res$predict.residuals, predict.res$residuals)
        
        dev.off()
        
        
        res %>% class()
        predict.res %>% class()
        predict.res
        
        
        
}


print(allresiduals)

allresiduals <- allresiduals %>% data.frame()

colnames(allresiduals) <- c("algorithms", "size", 
                            "gompertz pre RES", "gompertz RES"
)


allresiduals.texpath <- paste("./analysis-compare/tsp_parameters_prediction/residuals_tsp_predict_parameters.tex")
allresiduals.csvpath <- paste("./analysis-compare/tsp_parameters_prediction/residuals_tsp_predict_parameters.csv")
writeTabular(allresiduals,
             file = allresiduals.texpath,
             caption = "Residuals Comparison On Predict Parameters",
             align = "c",
             wrap.as.table = TRUE,
             table.position = "hbtp",
             caption.position = "t",
             centering = TRUE
)

write.csv(allresiduals, allresiduals.csvpath, row.names = FALSE)


