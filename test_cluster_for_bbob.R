source("./base-funcs/maxsattsp_load_and_order_func.R")
library(fpc)
dat <- LoadBbob("./modelresults/LM.bbob.pre/bbob-log-y/100percentleft")
head(dat)


if(which(dat$residuals == 1000000) %>% length() != 0){
    dat <- dat[-which(dat$residuals == 1000000), ]
}
plot(dat[, c("func", "d")])
#head(dat)
f11.dat <- dat[grep("f11/", dat$instance_file%>% as.character), ]
#label <- f11.dat$func.lab %>% droplevels()
label <- do.call(rbind, strsplit(f11.dat$instance_file%>% as.character , "/"))[, 2]
CMASE.dat.Clustering <- select(f11.dat, a, b, c, d)
CMASE.dat.Clustering <- scale(CMASE.dat.Clustering)

?kmeans
dist( CMASE.dat.Clustering[1:10, ], method = 'manhattan')
#kc <- kmeans(CMASE.dat.Clustering, 5)
kc <- dbscan(CMASE.dat.Clustering, 0.7);  #分类模型训练  
#fitted(kc)  #查看具体分类情况  
table(label, kc$cluster)
plot(kc, f11.dat$a, CMASE.dat.Clustering[, c("c")] )
plot(kc,f11.dat[, c("c", "d")] )

plot(CMASE.dat.Clustering[, c("b", "c")] )

set.seed(665544)
n <- 600
x <- cbind(runif(10, 0, 10)+rnorm(n, sd=0.2), runif(10, 0, 10)+rnorm(n,
                                                                     sd=0.2))
par(bg="grey40")
ds <- dbscan(x, 0.2)