#pearson similar index
#cor(x, y, method = "pearson")
rm(list = ls())
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(cluster)

cosine_dist <- function(x, y){
    if(identical(x, c(0, 0, 0, 0)))
        return(0)
    if(identical(y, c(0, 0, 0, 0)))
        return(0)
    return(1 - x %*% y/(sqrt(x%*%x) * sqrt(y%*%y)))
    #  return(sum(abs(x-y)))
}



file <- "./modelresults/LM.bbob.pre/bbob-1-log-y/10_DCMP.csv"
dat <- LoadBbob(file)
dat <- dat[-which(dat[, "alg"] == "IPOP"), ]
dat <- dat[-which(dat[, "alg"] == "IPOP-500"), ]
dat <- dat[-which(dat[, "alg"] == "IPOP-tany"), ]
dat <- dat[-which(dat[, "alg"] == "IPOP-texp"), ]

#dat$a <-exp(dat$a)
dat <- dat[-which(dat[, "a"] == 0), ]
head(dat)


kmeans.funcs_no_pca <- function(dat){
    func.names <- dat$func %>% droplevels() %>% levels()
    func.list <- NULL
    for( fc in 1: length(func.names)){
        func.pattern <- paste(func.names[fc], "/", sep = "")
        subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
        figure.dat <-select(dat[subscript, ], c, d)
        figure.dat.names <- select(dat[subscript, ], alg)
        #   dist.mat <- proxy::dist(abs(figure.dat), method = cosine_dist)
        #   pamx <- pam(abs(dist.mat), 3)
        #   clusplot(pamx)
        k.means <-kmeans(figure.dat, 2)
        #  clusplot(pamx)
        xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
        func.list[[fc]] <- xx
    }
    names(func.list) <- func.names
    
    return (func.list)
}


kmeans.alg_no_pca <- function(dat){
    alg.names <- dat$alg %>% levels()
    alg.list <- NULL
    for(alg in 1: length(alg.names)){
        
        func.pattern <- paste(alg.names[alg], "/", sep = "")
        subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
        figure.dat <- select(dat[subscript, ], a, b, c, d)
        figure.dat.names <- select(dat[subscript, ], func)
        k.means <-kmeans(figure.dat, 2)
        xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
        alg.list[[alg]] <- xx
    }
    
    names(alg.list) <- alg.names
    
    return(alg.list)
}



kmeans.alg_pca <- function(dat){
    alg.names <- dat$alg %>% levels()
    alg.list <- NULL
    for(alg in 1: length(alg.names)){
        
        func.pattern <- paste(alg.names[alg], "/", sep = "")
        subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
        figure.dat.names <- select(dat[subscript, ], func)
        figure.dat <- select(dat[subscript, ],  a, b, c, d, residuals)
        library(stats)
        pca.test <- prcomp(figure.dat, scale = TRUE)
        summary(pca.test)
        pca.dat <- figure.dat %>% as.matrix()  %*% pca.test$rotation
        cluster.dat <- pca.dat[, c(1, 2, 3)]
        k.means <-kmeans(cluster.dat, 3)
        xx <-table(unlist(figure.dat.names), k.means$cluster)
        alg.list[[alg]] <- xx
    }
    
    names(alg.list) <- alg.names
    
    return(alg.names)
}

kmeans.func_pca <- function(dat){
    library(fpc)
    func.names <- dat$func %>% droplevels() %>% levels()
    fc <- 1
    func.list <- NULL
    for( fc in 1: length(func.names)){
        
        func.pattern <- paste(func.names[fc], "/", sep = "")
        subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
        figure.dat.names <- select(dat[subscript, ], alg)
        figure.dat <- select(dat[subscript, ],  a, b, c, d)
        library(stats)
        pca.test <- prcomp(figure.dat, scale = TRUE)
        summary(pca.test)
        pca.dat <- figure.dat %>% as.matrix()  %*% pca.test$rotation
        cluster.dat <- pca.dat[, c(1, 2, 3)]
        k.means <-kmeans(cluster.dat, 2)
        xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
        func.list[[fc]] <- xx
    }
    names(func.list) <- func.names
    
    return (func.list)
}


kmeans.func_pca(dat)
kmeans.funcs_no_pca(dat)
