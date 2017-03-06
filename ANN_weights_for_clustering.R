#pearson similar index
#cor(x, y, method = "pearson")
rm(list = ls())
source("./base-funcs/maxsattsp_load_and_order_func.R")
library(cluster)
file <- "./modelresults/LM.bbob.pre/bbob-singleRun-log-y/BL_10_LGMP.csv"
dat <- LoadBbob(file)
head(dat)

#subscript1 <- intersect(which(dat$dim == "DIM2"), which(dat$func == "f1"))
#subscript2 <-intersect(which(dat$alg == "CMAES"), which(dat$func == "f1"))
#subscript <- intersect(subscript1, subscript2)
subscript <- intersect(which(dat$alg == "CMAES"), which(dat$func == "f1"))
tmp.dat <- dat[subscript, ]; tmp.dat
#plot(tmp.dat[, 3], tmp.dat[, 8], col = tmp.dat$alg %>% droplevels() %>% as.numeric)
##observe clustering label
clustering.dat <- select(tmp.dat, a, b, c, d); clustering.dat
plot(clustering.dat[, 1], clustering.dat[, 2], col = tmp.dat$dim %>% droplevels() %>% as.numeric )
clustering.label <- tmp.dat$alg %>% droplevels()
clustering.label
# 
# k.means <- kmeans(clustering.dat, 6)
# kmeans.xx <- table(unlist(clustering.label), k.means$cluster %>% as.vector)
# 
# 
# pamx <- pam(clustering.dat, diss = FALSE, 6)
# pamx.xx <- table(unlist(clustering.label), pamx$clustering %>% as.vector)

library(stats)
pca.res <- prcomp(clustering.dat, scale = T)
summary(pca.res)
pca.dat <- scale(clustering.dat) %>% as.matrix()  %*% pca.res$rotation
plot(pca.dat[, 1], pca.dat[, 2], col = tmp.dat$dim %>% droplevels() %>% as.numeric )

k.means <- kmeans(pca.dat[, 1:11], 6)
kmeans.xx <- table(unlist(clustering.label), k.means$cluster %>% as.vector)

pamx <- pam(pca.dat[, 1:11], diss = FALSE, 6)
pamx.xx <- table(unlist(clustering.label), pamx$clustering %>% as.vector)

pamx.xx

library(fpc)
?dbscan
db.res <- dbscan(pca.dat[, 1:11], eps = 0.01,  method = "raw", showplot = 1)
plot(pca.dat[, 1], db.res)
 db.res$cluster
 
 
 plot( pca.dat[, 2], pca.dat[, 3])
