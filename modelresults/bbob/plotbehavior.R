rm(list = ls())
source("./bbob/base_var.R")
all.file = list.files("./rawdata/bbob/")

for(alg in 1:length(algorithms.bbob)){
      
        data.list =  all.file[grep(algorithms.bbob[alg], all.file)]
        pdfname = paste("./bbob/", algorithms.bbob[alg], ".pdf", sep = "")
        pdf(pdfname)
        par(mfrow = c(2,2) )
        for( j in 1:length(data.list)){
                data.path = paste("./rawdata/bbob/", data.list[j], sep = "")
                data = read.csv(data.path)
                plot(data$x, data$y, xlab = "FEs", log = "xy", ylab = "Fitness Value", main = gsub(".csv", "", data.list[j]),
                     pch = 20, cex = 1, col = "blue")
                grid()
        }
        
        
        dev.off()
}




data = read.csv("./rawdata/maxsat/1FlipHCrs_uf250-02.csv")
plot(data$x, data$y, xlab = "FEs", log = "xy", ylab = "Fitness Value", main = gsub(".csv", "", data.list[j]),
     pch = 20, cex = 1, col = "blue")