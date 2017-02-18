rm(list = ls())

#generate parameters setting affect on  residuals curve
library(dplyr)
#library(data.table)

file.path = "./modelresults/NN.analysing/maxsat/"

list.names = file.path %>% list.files()

list.names = list.names[grep("20iter_", list.names)]

XX <- list()
for(i in 1:length(list.names)){
        
        XX[[i]]<-  file.path %>% paste(list.names[i],sep = "") %>% read.csv(header = TRUE)
}

res <- Reduce(function(x, y) merge(x, y, all=TRUE,by ="algorithms_instances_names" ), XX)

head(res)

#descrbe hiddensize changes affects.
hiddensize = c( 6, 8, 10)
row.names = NULL
hid.averdata = NULL
mean.residuals = NULL
for( hid in 1: length(hiddensize)){
        select.hid <- paste("_", hiddensize[hid],"hiddensize_", sep = "")
        row.names <- cbind(row.names,select.hid)
        mean.residuals <- mean.residuals %>% rbind(select(res, matches(select.hid)) %>% apply(2, mean), deparse.level = 0)
}
col.names <- gsub("X20iter_2hiddensize_", "",colnames(mean.residuals))
row.names <- gsub("_", "", row.names)
rownames(mean.residuals) <- row.names
colnames(mean.residuals) <- col.names


setEPS()
names = "maxsat_NN_Approximator_Performance_Analysis.eps"# pic name
postscript(names)

for(pt in 1:nrow(mean.residuals)){
        
        if(pt == 1) {
                plot(c(1:ncol(mean.residuals)), mean.residuals[pt, ], type = "b", axes = FALSE,
                     ylim = c(min(mean.residuals)-1, max(mean.residuals)+1),
                     pch = pt+1,lwd = 3,col = pt+1,
                     xlab = "ANN Iterator Times", ylab = "Residuals"
                     )
        } else {
                points(c(1:ncol(mean.residuals)), mean.residuals[pt, ], type = "b",
                     pch = pt+1, lwd = 3, col = pt+1)
        }
        
        
}
axis(1,1:ncol(mean.residuals), labels = col.names)
axis(2)
box()
grid(lwd = 2)
legend("topright",legend = row.names, pch = c(2:6), col = c(2:6), lwd = 2, cex = 0.7)
title()

dev.off()


