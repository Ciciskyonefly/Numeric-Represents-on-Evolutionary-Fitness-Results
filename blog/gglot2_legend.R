rm(list = ls())
library(ggplot2)
library(dplyr)
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/ggplot2_theme_label.R")

list.model <- list(decayModelpositive)

file.path<- "./rawdata/maxsat/mFlipHCrs_uf075-04.csv"
para.path <- "./modelresults/LM.maxsat.pre/100percentleft/10_logisticModelpositive_model.csv"



gsub("_","/","mFlipHCrs_uf250-04")

rawdata <- read.csv(file.path, header = TRUE)
parameters.data <- LoadMaxsat(para.path)

theta <- parameters.data[grep(gsub("_","/","mFlipHCrs_uf075-04"), parameters.data$instance_file), ] %>% 
        select(a,b,c,d) %>%
        as.vector() %>% 
        as.numeric()

colours= rainbow(7)[3:7]


#rawdata = seq(1, 10000, by = 0.1)

i = 1

alldata <- NULL
for ( i in 1:5) {
        
        line.data <- GetFormulaValue(logisticModelpositive$formula, rawdata[, 1], theta)
        names(line.data) <- c("x", "y")
        line.data$variable <- i
        
              
        alldata = alldata %>% rbind(line.data)
        
        theta[3] <- theta[3] - 0.02
        theta[4] <-  theta[4] - 0.1
}

qipa <- ggplot(alldata, aes(x, y, color = factor(variable)) )+ scale_x_log10() + geom_line(size = 1) 
qipa <- qipa + scale_color_brewer( labels = c("a", "b", "c", "d", "e"), palette =  1)  + guides( color = guide_legend( title = "Parameter Dimense") ) 
qipa <- qipa + theme(
        panel.background = element_rect(fill = "white", colour = "black" ),
        panel.grid.major = element_line( linetype = 3 , colour = "lightgrey" ),
        panel.grid.minor = element_blank(),
        legend.title = element_text( vjust = 1)
) + labs( 
        x = "Run Time",
        y = "Performace",
        colour = "Cylinders" )
plot(qipa)





bp + scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
                      labels=c("Control", "Treat 1", "Treat 2"))

