rm(list = ls())
library(cluster)
source("./base-funcs/maxsattsp_load_and_order_func.R")


visuliazationOfDimDifference <- function(dat, pdf.name){
    split.dat <- split(dat, list(dat$alg, dat$func))
    pdf(pdf.name)
    par(mfrow = c(2, 2))
    for(spl in 1:length(split.dat)){
        if(nrow(split.dat[[spl]]) == 0) next
        tmp.dat <- split.dat[[spl]]
        a.lim <- c(max(-10, sort(exp(tmp.dat[, "a"]))[4]), min(10, sort(exp(tmp.dat[, "a"]), decreasing = TRUE)[4]))
        b.lim <- c(sort(log10(tmp.dat[, "b"]))[4], sort(log10(tmp.dat[, "b"]), decreasing = TRUE)[4])
        c.lim <- c(sort(tmp.dat[, "c"])[4], sort(tmp.dat[, "c"], decreasing = TRUE)[4])
        d.lim <- c(sort(tmp.dat[, "d"])[4], sort(tmp.dat[, "d"], decreasing = TRUE)[4])
        
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "c"], col = tmp.dat$dim %>% droplevels() %>% as.numeric,
             xlim = a.lim, ylim = c.lim, pch = 20, 
             xlab = "a", ylab = "c")
        title( main = names(split.dat[spl]))
        
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "d"], col = tmp.dat$dim %>% droplevels() %>% as.numeric, 
             xlim = a.lim, ylim = d.lim, pch = 20,
             xlab = "a", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(tmp.dat[, "c"], tmp.dat[, "d"], col = tmp.dat$dim %>% droplevels() %>% as.numeric,
             xlim = c.lim, ylim = d.lim, pch = 20,
             xlab = "c", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "b"], col = tmp.dat$dim %>% droplevels() %>% as.numeric,
             xlim = a.lim,  pch = 20,
             xlab = "a", ylab = "b")
        title( main = names(split.dat[spl]))
        
    }
    dev.off()
}


visulizationaOfAlgorithmDifference <- function(dat, pdf.name){
    
    split.dat <- split(dat, list(dat$func, dat$dim))
    pdf(pdf.name)
    par(mfrow = c(2, 2))
    for(spl in 1:length(split.dat)){
        if(nrow(split.dat[[spl]]) == 0) next
        tmp.dat <- split.dat[[spl]]
        a.lim <- c(max(-10, sort(exp(tmp.dat[, "a"])[4])), min(10, sort(tmp.dat[, "a"], decreasing = TRUE)[4]))
        b.lim <- c(sort(log10(tmp.dat[, "b"]))[4], sort(log10(tmp.dat[, "b"]), decreasing = TRUE)[4])
        c.lim <- c(sort(tmp.dat[, "c"])[4], sort(tmp.dat[, "c"], decreasing = TRUE)[4])
        d.lim <- c(sort(tmp.dat[, "d"])[4], sort(tmp.dat[, "d"], decreasing = TRUE)[4])
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "c"], col = tmp.dat$alg %>% droplevels() %>% as.numeric,
             xlim = a.lim, ylim = c.lim, pch = 20, 
             xlab = "exp.a", ylab = "c")
        title( main = names(split.dat[spl]))
        
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "d"], col = tmp.dat$alg %>% droplevels() %>% as.numeric, 
             xlim = a.lim, ylim = d.lim, pch = 20,
             xlab = "exp.a", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(exp(tmp.dat[, "c"]), tmp.dat[, "d"], col = tmp.dat$alg %>% droplevels() %>% as.numeric,
             xlim = c.lim, ylim = d.lim, pch = 20,
             xlab = "c", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(exp(tmp.dat[, "a"]), tmp.dat[, "b"], col = tmp.dat$alg %>% droplevels() %>% as.numeric,
             xlim = a.lim,  pch = 20,
             xlab = "exp.a", ylab = "b")
        title( main = names(split.dat[spl]))
        
    }
    dev.off()
    
}


visulizationaOfFuncDifference <- function(dat, pdf.name){
    
    split.dat <- split(dat, list(dat$alg))
    pdf(pdf.name)
    par(mfrow = c(2, 2))
    for(spl in 1:length(split.dat)){
        if(nrow(split.dat[[spl]]) == 0) next
        tmp.dat <- split.dat[[spl]]
        a.lim <- c(max(-10, sort(tmp.dat[, "a"])[2]), min(10, sort(tmp.dat[, "a"], decreasing = TRUE)[2]))
        b.lim <- c(sort(log10(tmp.dat[, "b"]))[2], sort(log10(tmp.dat[, "b"]), decreasing = TRUE)[2])
        c.lim <- c(sort(tmp.dat[, "c"])[2], sort(tmp.dat[, "c"], decreasing = TRUE)[2])
        d.lim <- c(sort(tmp.dat[, "d"])[2], sort(tmp.dat[, "d"], decreasing = TRUE)[2])
        
        plot(tmp.dat[, "a"], tmp.dat[, "c"], col = tmp.dat$func %>% droplevels() %>% as.numeric,
             xlim = a.lim, ylim = c.lim, pch = 20, 
             xlab = "a", ylab = "c")
        title( main = names(split.dat[spl]))
        
        plot(tmp.dat[, "a"], tmp.dat[, "d"], col = tmp.dat$func %>% droplevels() %>% as.numeric, 
             xlim = a.lim, ylim = d.lim, pch = 20,
             xlab = "a", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(tmp.dat[, "c"], tmp.dat[, "d"], col = tmp.dat$func %>% droplevels() %>% as.numeric,
             xlim = c.lim, ylim = d.lim, pch = 20,
             xlab = "c", ylab = "d")
        title( main = names(split.dat[spl]))
        
        plot(tmp.dat[, "a"], tmp.dat[, "b"], col = tmp.dat$func %>% droplevels() %>% as.numeric,
             xlim = a.lim,  pch = 20,
             xlab = "a", ylab = "log10.b")
        title( main = names(split.dat[spl]))
        
    }
    dev.off()
    
}


file <- "./modelresults/LM.bbob.pre/bbob-singleRun-log-y/"
list.files <- list.files(file)
pdf.list <- paste("./visualization/alg_", gsub(".csv", ".pdf", list.files), sep = "")
for( i in 6:length(pdf.list)){
    dat <- LoadBbob(paste(file, list.files[i], sep = ""))
    if(which(dat[, "a"] == 0) %>% length != 0){
        dat <- dat[-which(dat[, "a"] == 0), ]
    }
    visulizationaOfAlgorithmDifference(dat, pdf.list[i])
    #visuliazationOfDimDifference(dat, pdf.list[i])
}