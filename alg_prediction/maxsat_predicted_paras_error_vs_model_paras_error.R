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
pre.notSR <- 0
model.notSR <- 0
SR <- 4
allresiduals <- NULL
count <- 0
list.model <- list(logisticModelpositive)
loc.path = "./modelresults/LM.maxsat.pre/100percentleft/"
all.path <- loc.path %>% list.files()
prediction.path <- "./analysis-compare/maxsat_parameters_prediction/"
prediction.file <- prediction.path %>% list.files()

for(mod in 1:length(list.model)){
        
      #prediction data parameters
       data.prediction <- paste(prediction.path, prediction.file[grepl(list.model[[mod]]$name, prediction.file)], sep = "") %>% read.csv()
       parameters.dat <- dplyr::select(data.prediction, a, b, c, d)
       path = "./rawdata/maxsat/"
       csvnames <- path %>% list.files()
       pre.predict.resdiuals <- NULL
       model.predict.residuals <- NULL
       list_plot = list()
       
       
       #model parameters
       model.pattern <- paste("_", list.model[[mod]]$name, ".+.csv", sep = "")
       file.path <- paste(loc.path, all.path[grep(model.pattern, all.path)], sep = "")
       model.data <- LoadMaxsat(file.path)
       model.data <- model.data[-grep("experiment", model.data$instance_file%>% as.character), ]
       mean.model.data <-  aggregate(cbind(a, b, c, d) ~  instances + algorithms, data = model.data, FUN = mean)
       
       #pdfname <- paste( list.model[[mod]]$name, "_predict_line.pdf")
       for(dat.i in 1: nrow(parameters.dat)){
               
               #找所在位置
               pre.predict_error <- 0
               model.predict_error <- 0
               if(data.prediction$size[dat.i] < 100){
                       pattern <- paste(data.prediction$algorithms[dat.i],"_.+0", data.prediction$size[dat.i], "-", sep = "")
               } else {
                       pattern <- paste(data.prediction$algorithms[dat.i],"_.+", data.prediction$size[dat.i], "-", sep = "")
               }
               
               temp.listnames <- csvnames[grep(pattern, csvnames)]
               for(each in 1:length(temp.listnames)){
                       
                       data.path = paste(path, temp.listnames[each], sep = "")
                       #print(data.path)
                       raw.data = read.csv(data.path, header = TRUE)
                       
                       pre.predict_y <- list.model[[mod]]$modelFunction(parameters.dat[dat.i, ] %>% t() %>% as.numeric(), xData = raw.data$x)
                       pre.predict_error <- xyRMSE(raw.data$y, pre.predict_y) + pre.predict_error
                       
                       model.predict_y <- list.model[[mod]]$modelFunction(mean.model.data[dat.i, c("a", "b", "c", "d")] %>% t %>% as.numeric, xData = raw.data$x)
                       model.predict_error <- xyRMSE(raw.data$y, model.predict_y) + model.predict_error
                      
                       #plot section/not need now
                       #pic = list.model[[mod]]$plotFunction(raw.data, parameters.dat[dat.i, ] %>% t() %>% as.numeric())
                       #temp.name <- strsplit(temp.listnames[each], "\\.") %>% unlist()
                       #pic.names <- paste( list.model[[mod]]$name, temp.name[1])
                       #pic <- pic + labs(title = pic.names) + theme(plot.title = element_text(hjust = 0.5))
                       # model.parameters <- model.data[grepl(gsub("_", "/", temp.name[1]), model.data$instance_file), ] %>% dplyr::select(a,b,c,d) %>% t() %>% as.numeric()
                       # model_y <- list.model[[mod]]$modelFunction(model.parameters, xData = raw.data$x)
                       # model_X <- raw.data$x
                       # model.line <- data.frame(model_X, model_y)
                       # pic <- pic + geom_line(mapping = aes(model_X, model_y), data = model.line)
                       # count <- count + 1
                       # list_plot[[count]] <- pic
                       
                       
                       if(xyRMSE(raw.data$y, pre.predict_y) > SR){
                         pre.notSR <- pre.notSR + 1
                         cat("Pre:\n")
                         print(data.path)
                         print(xyRMSE(raw.data$y, pre.predict_y))
                       }
                       if(xyRMSE(raw.data$y, model.predict_y) > SR){
                         model.notSR <- model.notSR + 1
                         cat("Model:\n")
                         print(data.path)
                         print(xyRMSE(raw.data$y, model.predict_y))
                       }
                       
                  count <- count + 1    
               }
               
               pre.aver_error <- pre.predict_error/(length(temp.listnames))
               model.aver_error <- model.predict_error/(length(temp.listnames))
               pre.predict.resdiuals <- pre.predict.resdiuals %>% rbind(round(pre.aver_error, 5))  
               model.predict.residuals <- model.predict.residuals %>% rbind(round(model.aver_error, 5))
       }
       
       
       
       
       start.time <- Sys.time()
       
       # library(gridExtra)
       # ggsave(filename = pdfname,  marrangeGrob(grobs = list_plot, nrow = 2, ncol = 2))
       # 
       end.time <- Sys.time()
       print(end.time - start.time)
       
       
       
       #generate table
       # predict.res <- cbind(data.prediction, predict.resdiuals)
       # residuals <- model.data[, "residuals"]
       # alldata <- LoadMaxsat(file.path)
       # residual.data <-  dplyr::select(cbind(alldata, residuals), algorithms, instances,  residuals)
       # res <- aggregate(residual.data$residuals, residual.data[, -3], mean)
       # res$algorithms
       # res <- res[order(res$algorithms), ]
       # colnames(res) <- c("algorithms", "size", "resdiuals")
       # 
       # exp.sup <- grep("experiment", res$size)
       # if(exp.sup %>% length() !=0 ){
       #         res <- res[-exp.sup, ]
       # }
       # predict.res$residuals <- res$resdiuals
       # 
       # pp <- split(predict.res, predict.res$algorithms)
       # 
       # pdfname <- paste(list.model[[mod]], "_residuals.pdf")
       # pdf(pdfname)
       # for(i in 1:length(pp)){
       #         
       #         plot(1:nrow(pp[[i]]), pp[[i]]$predict.resdiuals, 
       #              type = "b",
       #              col = "red", pch = 3, size = 2,
       #              ylim = c(0, 5), axes = FALSE,
       #              xlab = "Instances size",
       #              ylab = "Residuals")
       #         lines(1:nrow(pp[[i]]), pp[[i]]$residuals, type = "b")
       #         axis(1, 1:nrow(pp[[i]]), labels = pp[[i]]$size)
       #         axis(2)
       #         box()
       #         grid()
       #         title(main = names(pp)[i])
       #         legend("topleft", legend = c("Predict", "No-Predict"))
       # }
       #dev.off()
       
       if(mod == 1){
               allresiduals <- allresiduals %>% cbind(data.prediction$algorithms %>% unfactor(), data.prediction$size)
       }
       
       allresiduals <- allresiduals %>% cbind(pre.predict.resdiuals, model.predict.residuals)
       
     
       
       
       
}


#print(allresiduals)

allresiduals <- allresiduals %>% data.frame()

colnames(allresiduals) <- c("algorithms", "size", 
                            # "decayPos.pre.RES", "decayPos.RES",
                            # "explinearPos.pre.RES", "explinearPos.RES",
                            "logisticPos.pre.RES", "logisticPos.RES"
                            # "decayNeg.pre.RES", "decayNeg.RES"
                            )


allresiduals.texpath <- paste("./analysis-compare/maxsat_parameters_prediction/maxsat_residuals_predict_parameters.tex")
allresiduals.csvpath <- paste("./analysis-compare/maxsat_parameters_prediction/maxsat_residuals_predict_parameters.csv")
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

#all residuals.
allresiduals[41:50, ]
allresiduals
(1 - pre.notSR/count) %>% print
(1 - model.notSR/count) %>% print
count %>% print()
