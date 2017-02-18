# library(doParallel)
# cl <- makeCluster(2)
# #Remember: unless registerDoMC is called, foreach will not run in parallel. Simply loading the
# #doParallel package is not enough.
# registerDoParallel(cl)
# foreach(i = 1:4) %dopar% sqrt(i)



library(doParallel)
registerDoParallel(cores=3)
foreach(i=1:3) %dopar% print("Hello World.")

#show how many cores you use.
getDoParWorkers()


#test time
x <- iris[which(iris[, 5] != "Setosa"), c(1, 5)]
trials <- 10
ptime <- system.time({
        
        
        
        
        
})