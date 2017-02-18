library("dplyr")

file.path = "./compareAnalysisCode/statisticalproperty/"

name.list <- file.path %>% list.files()

stat.property <- c("Median", "_Mean", "SEMean", "Variance", "Stdev")


for(i in 1:length(stat.property)){
        
        ustc <- name.list[grepl(stat.property[i] %>% tolower(), name.list)]
        modelnames <-c("logistic positive", "decay positive", "decay negative", "explinear positive", "nn")
        
        
        loc.path <- paste(file.path, ustc, sep = "")
        stat.mean <- read.csv(loc.path[1], header = TRUE, stringsAsFactors = FALSE)
        
        stat.mean1 <- read.csv(loc.path[2], header = TRUE, stringsAsFactors = FALSE)
        
        
        all.mean <- stat.mean %>% rbind(stat.mean1)
        all.mean <-  cbind(modelnames, all.mean)
        
        ncol(all.mean)
        
        all.mean <- all.mean[, c(1, 3:11, 2)]
        
        library(ggplot2)
        boxplot(all.mean[, 11])
        
        for( r in  1: nrow(all.mean)){
                if(r == 1){
                        plot(c(7:10), all.mean[r, 8:11], type = "b", pch = r)
                } else {
                        lines(c(7:10), all.mean[r, 8:11], col = r, type = "b", pch = r)
                }
                
                
        }
        
        legend("topright", col = c(1:nrow(all.mean)), legend = modelnames, pch = c(1:nrow(all.mean)), lty = 1 , cex = 0.7,
               text.font = 4)
        title(main = stat.property[i])
}





