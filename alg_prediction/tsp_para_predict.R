rm(list = ls())
library(ggplot2)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")

start.time <- Sys.time()

loc.path = "./modelresults/LM.tsp.pre/singleRun/100percentleft/"
all.path <- loc.path %>% list.files()

list.model <- list(gompertzModelpositive)
data.file <- paste(loc.path, all.path[grep("10.+gompertzModelpositive.+csv", all.path)], sep = "")
alldata <- LoadTsp(data.file)

library(stringr)
alldata$instances <- str_extract_all(alldata$instances%>% as.character, "[0-9]+", simplify = TRUE ) %>% as.numeric()
alldata <- alldata[order(alldata$instances), ]
split.data <- split(alldata,alldata$algorithms)
alg.names <- names(split.data)
para.names <- c("a", "b", "c", "d")
pre.parameter <- NULL
for(sp in 1:length(split.data)){
  traindata_4_parameters <- dplyr:: select(split.data[[sp]], c(1, 3:6, 9))
  temp.pre.parameter <- traindata_4_parameters[, 1] %>% as.character()
  for(j in 1:4){
    traindata <- data.frame(x = traindata_4_parameters$instances, y = traindata_4_parameters[, j+1])
    # trainIndex <- createDataPartition(traindata$x, p = .8, 
    #                                   list = FALSE, 
    #                                   times = 1)
    test.instances <- c(14, 29, 76)
    
    test.index <- which(traindata$x %in% test.instances)
    
    mytrain <- traindata[-test.index, ]
    mytest <- traindata[test.index, ]
    
    nnetGrid <- expand.grid(.decay = c(0,0.5, 0.1), .size = c(4, 5, 6, 7))
    nnet.model <- caret::train(y ~ . ,
                               data = mytrain,
                               method = "nnet",
                               metric = "RMSE",
                               linout = TRUE,
                               print.every.n = 100,
                               tuneGrid = nnetGrid,
                               trace = FALSE,
                               nrounds = 100               
    )
    
    #when predict, newdata should be the same as data, except for label.
    pred_y <- predict(nnet.model, newdata = data.frame(x = traindata$x))
    
    
    newdata <- data.frame(x = traindata$x, y = pred_y)
    
    eps.names = paste("./analysis-compare/tsp_parameters_prediction/", "singleRun-para_predict_", 
                      alg.names[sp], "_", para.names[j], ".eps",  sep = "")
    setEPS()
    postscript(eps.names)
    
    p <- ggplot(data = traindata)
    p <- p + geom_point(mytrain, mapping =aes(x, y, colour = "Train Data"),  cex = 2)
    p <- p + geom_point(mytest, mapping =aes(x, y, colour = "Test Point"),  cex = 2 )
    p <- p + geom_point(newdata, mapping =aes(x, y), col = "red",  cex = 2) 
    p <- p + geom_line(newdata, mapping =aes(x, y, colour = "Prediction"), lwd = 1)
    p <- p + scale_color_manual(values = c("Train Data" = "Black", "Test Point" = "Blue", "Prediction" = "Red") )
    p <- p + guides( colour = guide_legend( title = "")) 
    p <- p + labs(x = "Instance Scale",
                  y = "Value Of Parameter")
    plot(p)
    
    dev.off()
    
    temp.pre.parameter <- cbind(temp.pre.parameter, newdata$y)
    
  }
  pre.parameter <- rbind(pre.parameter, temp.pre.parameter)
}

print(pre.parameter)
save.path <- paste("./analysis-compare/tsp_parameters_prediction/tsp_predict_parameters.csv" )
write.csv(pre.parameter, save.path, row.names = FALSE )

