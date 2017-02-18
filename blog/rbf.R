
library(dplyr)
library(data.table)





# library(ggplot2)
# library(e1071)
# source("./models_var.R")
# source("./models_func.R")
# source("./data_preprocessing/readdata.R")
# 
# file.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/ts0r0b/symmetric/berlin52"
# data.matrix = getTheMatrixData_TspSuite(file.path)
# head(data.matrix)
# data.matrix = data.matrix[order(data.matrix[, 1]), ]
# #data.matrix = data.matrix[sample.index,]
# fixxData = data.matrix[, 1]
# fixyData = data.matrix[, 3]
# xData <- fixxData
# yData <- fixyData
# train.x <- xData %>% data.matrix()
# train.y <- yData %>% data.matrix()

rbf <- NULL
Green <- function(x, c, delta){
        greenValue <- exp(-1.0 * sum((x - c)^2) / (2 * delta^2))
}



hiddenSize <- 2
# cols <- 5
# rows <- 7
# train.x <- matrix(runif(cols * rows), ncol = 1)
# train.y <- matrix(runif( cols * rows), ncol = 1)
 x <- seq(0,3.14*2, by = 0.01)
 y <- sin(x) + runif(length(x))
train.x <- x %>% data.matrix()
train.y <- y %>% data.matrix()

kmeans.parameters <- kmeans(train.x, hiddenSize)

init.centers <- kmeans.parameters$centers
init.delta <- kmeans.parameters$withinss/kmeans.parameters$size + 0.2

rbf$hiddenSize <- hiddenSize
rbf$inputSize <- ncol(train.x)
rbf$outputSize <- ncol(train.y)
rbf$numSample <- nrow(train.x)

rbf$c <- matrix(init.centers, ncol = ncol(train.x))
rbf$delta <- matrix(init.delta, nrow = 1) #delta of RBF
rbf$wts <- matrix(rnorm(rbf$hiddenSize * rbf$outputSize, 0,2), ncol = rbf$outputSize) *2 - 1 #weight of RBF
rbf$cost <- 0 
rbf$alpha <- 0.1  # learning rate (should not be large!)  




TrainRBF <- function(rbf, train.x, train.y){
        
        ## step 1: calculate gradient
        #size : 1*n
        delta_delta <- rep(0,rbf$hiddenSize) %>% matrix(nrow = 1)
        delta_center <- rep(0, rbf$inputSize * rbf$hiddenSize) %>% matrix(ncol = rbf$inputSize)
        delta_weight <-  rep(0,rbf$outputSize*rbf$hiddenSize) %>% matrix(ncol = rbf$outputSize) 
        rbf$cost <- 0
        num.sample <- nrow(train.x)
        for( i in 1:rbf$numSample){
                #size : 1*n
                green <- matrix(rep(0, rbf$hiddenSize), nrow = 1)
                
                ## Feed forward
                y = 0
                for(j in 1:rbf$hiddenSize){
                        green[1, j] <- Green(train.x[i, ], rbf$c[j, ], rbf$delta[j])
                }
                
                output <- green %*% rbf$wts 
                
                if(is.nan(output))
                        output <- 0
              #  cat(output, "\n")
                ## Back propagation
                # error  = y - output
                error <- -(output - train.y[i, ])
                # sum E
                rbf$cost <- rbf$cost + sum(error * error)
                
                #updata weight  hiddensize * 1 * 1 * outputSize
                delta_weight <- delta_weight + t(green) %*% error
                # delta2[j] = w[j, ] * (y - output)   delta2 = w * e
                delta2 = error %*% t(rbf$wts) * green
                for( j in 1:rbf$hiddenSize){
                        delta_center[j, ] = delta_center[j, ] + delta2[j] * (train.x[i, ] - rbf$c[j, ])/rbf$delta[j]^2
                        delta_delta[j] = delta_delta[j] + delta2[j] *sum((train.x[i, ] - rbf$c[j, ])^2)/rbf$delta[j]^3
                }
        }
        
        ## Step2 : update parameters
        rbf$cost <- 0.5 * rbf$cost/num.sample
        rbf$delta <- rbf$delta + rbf$alpha * delta_delta/num.sample
        rbf$c <- rbf$c + rbf$alpha * delta_center/num.sample
        rbf$wts <- rbf$wts + rbf$alpha * delta_weight/num.sample
        
        return (rbf)
}


## Start Train
cat("Step 2: Start training...\n")
maxIter <- 50
preCost <- 0
for(i in 1:maxIter) {
        
        cat("Iteration ", i, " ")
        rbf <- TrainRBF(rbf, train.x, train.y)
        cat("The cost is ", rbf$cost, "\n")
        
        
        curCost <- rbf$cost
        if(abs(curCost - preCost) < 1e-8 ) {
                Cat('Reached iteration termination condition and Termination now! \n')
                break 
        }  
         
}

###Visuliazation 
green <- matrix(rep(0, rbf$numSample * rbf$hiddenSize) , nrow = rbf$numSample)
for( i in 1: rbf$numSample){
        
        for(j in 1:rbf$hiddenSize){
                green[i, j] <- Green(train.x[i, ], rbf$c[j, ], rbf$delta[j])
        }
}

z <- green %*% rbf$wts
plot(train.x,train.y)
lines(train.x, z, col = "red")
