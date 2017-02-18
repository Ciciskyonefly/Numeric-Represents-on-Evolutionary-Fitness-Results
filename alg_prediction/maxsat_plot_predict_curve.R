#This procedure is used to plot predict parameters value compared with average value.
#data already produced.
#in "./analysis-compare/maxsat_parameters_prediction/logistic

rm(list = ls())
library(extrafont)
library(dplyr)
library(ggplot2)
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
start.time <- Sys.time()



#model parameters
loc.path = "./modelresults/LM.maxsat.pre/100percentleft/"
all.path <- loc.path %>% list.files()
list.model <- list(logisticModelpositive)
data.file <- paste(loc.path, all.path[grep("10.+logisticModelpositive.+csv", all.path)], sep = "")
alldata <- LoadMaxsat(data.file)

library(stringr)
model.parameters <- aggregate(cbind(a, b, c, d) ~ instances + algorithms, data = alldata, FUN = mean)
model.parameters <- model.parameters[-grep("experiment", model.parameters$instances %>% as.character), ]
alldata <- alldata[-grep("experiment", alldata$instances %>% as.character), ]

#predict parameters
file.path =  "./analysis-compare/maxsat_parameters_prediction/"
#show all file names in file.path  
#file.path %>% list.files()
file <- paste(file.path, "logisticModelpositive_predict_parameters.csv", sep = "")
pre.parameters <- read.csv(file)
pre.parameters$algorithms <- factor(pre.parameters$algorithms, levels = levels(model.parameters$algorithms), ordered = TRUE)



alldata.split <- split(alldata, alldata$algorithms)
model.para.split <- split(model.parameters, model.parameters$algorithms)
pre.para.split <- split(pre.parameters, pre.parameters$algorithms)

names(alldata.split)
names(model.para.split)
names(pre.para.split)
alg.names <- names(pre.para.split)
alg <- 1
for(alg in 1:length(alg.names)){ 
  temp.alldata <-  alldata.split[[alg]][c("instances", "a", "b", "c", "d")]
  #设置label名字为c("uf020"...."uf250")
  size <- temp.alldata$instances %>% unique
  mytrain <- temp.alldata[-which(temp.alldata$instances%in% c("uf150", "uf250")), ]
  mytest <- temp.alldata[which(temp.alldata$instances%in% c("uf150", "uf250")), ]
  temp.model <- cbind(size, model.para.split[[alg]][c("a", "b", "c", "d")])
  temp.pre <-  cbind(size, pre.para.split[[alg]][c("a", "b", "c", "d")])
  y.plot.label <- c("A", "B", "C", "D")
  para <- 1
  for(para in 1:4){
    y.label <- paste("Value of ", y.plot.label[para], sep = "")
    setEPS()
    postscript(paste("./alg_prediction/maxsat_parameters_prediction/", names(alldata.split)[alg],"_",y.plot.label[para], ".eps", sep = ""))
    #postscript("Hello.png")
    p <- ggplot(data = temp.alldata) 
    #colour 用来映射ggplot2的legend.
    p <- p + geom_point(mytrain, mapping = aes(instances, mytrain[, para+1],  colour = "Train Data"), cex = 2)
    p <- p + geom_point(mytest, mapping =aes(instances, mytest[, para+1], colour = "Test Data"),  cex = 2 )
    p <- p + geom_point(temp.pre, mapping =aes(size, temp.pre[, para+1]), col = "red",  cex = 2)
    p <- p + geom_point(temp.model, mapping =aes(size, temp.model[, para+1]), col = "green",  cex = 2) 
    p <- p + geom_line(temp.model, mapping =aes(size%>% as.numeric, temp.model[, para+1], colour = "Average"), lwd = 1)
    p <- p + geom_line(temp.pre, mapping =aes(size%>% as.numeric, temp.pre[, para+1], colour = "Prediction"), lwd = 1)
    #手动设置legend 的颜色
    p <- p + scale_color_manual(values = c( "Test Data" = "Blue", "Train Data" = "Black","Prediction" = "Red", "Average" = "Green") )
    #设置标题
    p <- p + guides( colour = guide_legend( title = ""))
    #横纵坐标
    p <- p + labs(x = "Instance Scale",
                  y = y.label)
    #p<- p + scale_x_discrete(names(temp.alldata$instances %>% unique), labels = c(1:10))
    #family 为字体 需要加载包library(extrafont)
    p <- p + theme( 
      aspect.ratio = 1,
      #grid 线型linetype, 颜色 colour. 有大小grid 之分 区别自己试
      panel.grid.major = element_line(linetype = 3 , colour = "lightgrey" ),
      panel.grid.minor = element_blank(),
      #主题 ggplot2 默认为灰色
      panel.background = element_rect(fill = "white", colour = "black"),
      #横坐标大小size, 角度angle, 字体family
      axis.text.x = element_text(size = 15, angle = 90, family = "Arial"),
      axis.text.y = element_text(size = 15, hjust = 1, family = "Arial"),
      #大小size，字体 family
      axis.title.x = element_text(size = 15, family = "Arial"),
      axis.title.y = element_text(size = 15, family = "Arial"),
      #legend 大小size, 字体family
      legend.text = element_text(size = 15, family = "Arial")
      
    )

    plot(p)
    dev.off()
    
  }
  
    
}
