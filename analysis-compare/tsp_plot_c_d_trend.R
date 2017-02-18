rm (list = ls()) 
source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
library(dplyr)

OrderInstance.tsp <- function(instances){
        library(stringr)
        instances <- unique(instances)
        numIns = str_extract_all(instances,"[0-9]+")
        x <- do.call(rbind,numIns)
        dataOrderMatrix <-cbind(as.vector(instances),unlist(numIns))
        colnames(dataOrderMatrix) <- c("datafile","num")
        dataOrderMatrix = dataOrderMatrix[order(dataOrderMatrix[,2]),]
        return(dataOrderMatrix[,"datafile"])
}

OrderAlgorithms.tsp <- function(algorithm){
        # Reorder algorithms name 
        #
        # Args:
        #   algorithm: A vector of list names.
        
        uni.algorithm <- unique(algorithm)
        uni.algorithm <- sort(uni.algorithm)
        uni.algorithm.0b = uni.algorithm[grep("0b",uni.algorithm)]
        uni.algorithm.0f = uni.algorithm[grep("0f",uni.algorithm)]
        uni.algorithm = c(uni.algorithm.0b,uni.algorithm.0f)
        return(uni.algorithm)
}


loc.path = "./modelresults/LM.tsp.pre/100percentleft/"
all.path <- loc.path %>% list.files()
list.model <- list(gompertzModelpositive)
data.file <- paste(loc.path, all.path[grep("10_gompertzModelpositive.+csv", all.path)], sep = "")
alldata <- LoadTsp(data.file)

alldata$bin.algorithms <- NULL
alldata[grep("0b",alldata$algorithms %>% as.character), "bin.algorithms"] <-"0b"
alldata[-grep("0b",alldata$algorithms %>% as.character), "bin.algorithms"] <-"0f"
alldata$bin.algorithms <- alldata$bin.algorithms %>% factor()

c_d_mean <- aggregate(cbind(c,d)~instances+bin.algorithms, data = alldata, FUN = mean)


setEPS()
names = "./analysis-compare/_tsp_gompertzModelpositive_c.eps"  # pic name
postscript(names)
ylim = c(min(c_d_mean$c), max(c_d_mean$c))
plot(c(1:(nrow(c_d_mean)/2)), c_d_mean$c[c(1:(nrow(c_d_mean)/2))], type = "b", pch = 20,  bg = "blue", ylim = ylim, axes = FALSE, lwd= 2,xlab = " ",ylab = "",col = "BLUE") # plot pic with no x,y axis
points(c(1:(nrow(c_d_mean)/2)),c_d_mean$c[-c(1:(nrow(c_d_mean)/2))], type = "b", bg = "black", pch = 20, lwd= 2, xlab = " ",ylab = " ", lty = 2, col = "black") # plot pic with no x,y axis
axis(1,1:(nrow(c_d_mean)/2), c_d_mean$instances[1:(nrow(c_d_mean)/2)],las = 2)
axis(2)
box()
legend("bottomleft", pch = c(20, 20), lty = c(1, 2), col = c("blue", "black"), legend = c("b", "f"), lwd = 1, cex =1.2)
dev.off()

setEPS()
names = "./analysis-compare/_tsp_gompertzModelpositive_d.eps"  # pic name
postscript(names)
ylim = c(min(c_d_mean$d), max(c_d_mean$d))
plot(c(1:(nrow(c_d_mean)/2)), c_d_mean$d[c(1:(nrow(c_d_mean)/2))], type = "b", pch = 20,  bg = "blue", ylim = ylim, axes = FALSE, lwd = 2, xlab = " ", ylab = "", col = "BLUE") # plot pic with no x,y axis
points(c(1:(nrow(c_d_mean)/2)), c_d_mean$d[-c(1:(nrow(c_d_mean)/2))], type = "b", bg = "black", pch = 20, lwd= 2, xlab = " ",ylab = " ", lty = 2, col = "black") # plot pic with no x,y axis
axis(1,1:(nrow(c_d_mean)/2), c_d_mean$instances[1:(nrow(c_d_mean)/2)],las = 2)
axis(2)
box()
legend("topright", pch = c(20, 20), lty = c(1, 2), col = c("blue", "black"), legend = c("b", "f"), lwd = 1, cex = 1.2)
dev.off()
