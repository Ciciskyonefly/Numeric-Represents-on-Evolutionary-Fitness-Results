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



file <- "./modelresults/LM.bbob.pre/bbob-log-y/100percentleft/BL_50_100_log_10_LGMP.csv"
dat <- LoadBbob(file)
#dat$a <-exp(dat$a)
dat <- dat[-which(dat[, "a"] == 0), ]
head(dat)

func.names <- dat$func %>% droplevels() %>% levels()
fc <- 1
func.list <- NULL
for( fc in 1: length(func.names)){
    
    func.pattern <- paste(func.names[fc], "/", sep = "")
    subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
    figure.dat <- select(dat[subscript, ], b, c)
    figure.dat.names <- select(dat[subscript, ], alg)
    #    dist.mat <- proxy::dist(abs(figure.dat), method = cosine_dist)
    #    pamx <- pam(abs(dist.mat), 3)
    #   clusplot(pamx)
    k.means <-kmeans(figure.dat, 2)
    #  clusplot(pamx)
    xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
    func.list[[fc]] <- xx
}
names(func.list) <- func.names

alg.names <- dat$alg %>% levels()
alg.list <- NULL
for(alg in 1: length(alg.names)){
    
    func.pattern <- paste(alg.names[alg], "/", sep = "")
    subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
    figure.dat <- select(dat[subscript, ], b, c)
    figure.dat.names <- select(dat[subscript, ], func)
    #    dist.mat <- proxy::dist(abs(figure.dat), method = cosine_dist)
    #    pamx <- pam(abs(dist.mat), 3)
    #   clusplot(pamx)
    k.means <-kmeans(figure.dat, 3)
    xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
    alg.list[[alg]] <- xx
}

names(alg.list) <- alg.names

alg.list
func.list$f2


# plot(pamx)
# library(fpc)
# figure.dat <- select(dat, a, b, c, d, residuals)
# figure.dat.names <- select(dat, alg)
# dist.mat <- proxy::dist(figure.dat[1:5, ], method = cosine_dist)
# pamx <- pam(abs(dist.mat), 2)
# table(unlist(figure.dat.names), pamx$clustering %>% as.vector)
# db.res <- dbscan(dist.mat, eps = 0.02,  method = "dist", showplot = 1)
# plot(figure.dat[, "a"], db.res)
# db.res$cluster
#?kmeans

alg.names <- dat$alg %>% levels()
alg.list <- NULL
for(alg in 1: length(alg.names)){
    
    func.pattern <- paste(alg.names[alg], "/", sep = "")
    subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
    figure.dat.names <- select(dat[subscript, ], func)
    figure.dat <- select(dat[subscript, ],  a, b, c, d, residuals)
    library(stats)
    pca.test <- prcomp(figure.dat, scale = TRUE)
    summary(pac.test)
    pca.dat <- figure.dat %>% as.matrix()  %*% pac.test$rotation
    cluster.dat <- pca.dat[, c(1, 2, 3)]
    k.means <-kmeans(cluster.dat, 3)
    xx <-table(unlist(figure.dat.names), k.means$cluster)
    alg.list[[alg]] <- xx
}

names(alg.list) <- alg.names

func.names <- dat$func %>% droplevels() %>% levels()
fc <- 1
func.list <- NULL
for( fc in 1: length(func.names)){
    
    func.pattern <- paste(func.names[fc], "/", sep = "")
    subscript <- grep(func.pattern, dat$instance_file %>% as.vector)
    figure.dat.names <- select(dat[subscript, ], alg)
    figure.dat <- select(dat[subscript, ],  a, b, c, d, residuals)
    library(stats)
    pca.test <- prcomp(figure.dat, scale = TRUE)
    summary(pac.test)
    pca.dat <- figure.dat %>% as.matrix()  %*% pac.test$rotation
    cluster.dat <- pca.dat[, c(1, 2, 3)]
    k.means <-kmeans(cluster.dat, 10)
    xx <- table(unlist(figure.dat.names), k.means$cluster %>% as.vector)
    func.list[[fc]] <- xx
}
names(func.list) <- func.names
func.list
# biplot(pac.test)
# plot(pac.test)
# head(figure.dat)
# head(figure.dat)
