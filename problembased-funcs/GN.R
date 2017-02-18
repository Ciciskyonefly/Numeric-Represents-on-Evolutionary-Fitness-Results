source("./models_var.R")
source("./models_func.R")



nlsSolverSingleModel <- function(model,xData,yData){
        
        slmres = NULL 
        slmres$residual = 1000000
        slmres$par = model$initFunc(xData,yData)
        
        nls.res = NULL
        nls.res$residual = 1000000
        nls.res$par <- model$initFunc(xData,yData)

        x = xData
        y = yData  
        y <- abs(y)
        if(min(y) <= 0){
                yy= y
                yy = yy[-which(yy <= 0)]
                y[which(y == 0)] = (min(yy)/2) 
        }
   
        
        for(pp in 1:model$iter){
                
                initialPara <- getInitialParameters(model, xData, yData)
                nls.res$par =  initialPara
                #nls.res$residual <- xyRMSE(model$formula,xData,yData,nls.res$par)
                nls.res$residual <- 1000000
                
                tryCatch({
                      #  print(1)
                        startlist = initialPara
                        datalist  = list(y=y,x=x)
                        fit = nls(model$formula,data =datalist,start = model$nlsstart(startlist),control = list(maxiter = 100), weights = (1/y))
                        nls.res$par = as.vector(coef(fit))
                        nls.res$residual = xyRMSE(model$formula,xData,yData,nls.res$par)
                        #print(nls.res$residual)
                        pred_y = predict(fit,xData)
                        least_square = sum((pred_y - y)*(pred_y-y)/y)
                },error = function(e){
                })
                
                if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)
                
                #print(nls.res$residual)
                # para_judge_large = (nls.res$par > 100)
                # t = length(which(para_judge_large == FALSE))
                # if(t!= model$nParameters){
                #         cat("judge large number\n")
                #         nls.res$residual <- 1000000
                #         next
                # }
                        
                #cat(para_judge_large)
                
                flag = model$flag * nls.res$par[2]
                
                if(flag<=0) next
                
                if(nls.res$residual < slmres$residual ){
                        slmres$residual  = nls.res$residual
                        slmres$par = nls.res$par
                }
                # if(slmres$residual < error)
                #         break
        }
        
        if(slmres$residual >= 1000000){
                
                for(pp in 1:model$iter){
                        
                        initialPara <- model$initFunc(xData,yData)
                        nls.res$par =  initialPara
                        #nls.res$residual = xyRMSE(model$formula,xData,yData,nls.res$par)
                        nls.res$residual <- 1000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nls(model$formula,data =datalist,start = model$nlsstart(startlist),control = list(maxiter = 100), weights = (1/y))
                                nls.res$par = as.vector(coef(fit))
                                nls.res$residual = xyRMSE(model$formula,xData,yData,nls.res$par)
                                pred_y = predict(fit,xData)
                                least_square = sum((pred_y - y)*(pred_y-y)/y)
                        },error = function(e){
                        })
                        
                        
                        

                        para_judge_large = (nls.res$par > 100)


                        # t = length(which(para_judge_large == FALSE))
                        # if(t!= model$nParameters){
                        #         cat("judge large number\n")
                        #         nls.res$residual <- 1000000
                        #         next
                        # }

                        
                        
                        if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)
                        
                        flag = model$flag * nls.res$par[2]
                        
                        if(flag<=0){
                                nls.res$residual = 1000000
                                next
                        }
                        if(nls.res$residual < slmres$residual ){
                                slmres$residual  = nls.res$residual
                                slmres$par = nls.res$par
                        }
                        # if(slmres$residual < error)
                        #         break
                }
        }
        
        
        if(slmres$residual >= 1000000){
                
                for(pp in 1:model$iter){
                        
                        initialPara <- rnorm(model$nParameters)
                        nls.res$par <- initialPara
                        #nls.res$residual = xyRMSE(model$formula,xData,yData,nls.res$par)
                        nls.res$residual <- 1000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nls(model$formula,data =datalist,start = model$nlsstart(startlist),control = list(maxiter = 100), weights = (1/y))
                                nls.res$par = as.vector(coef(fit))
                                nls.res$residual = xyRMSE(model$formula,xData,yData,nls.res$par)
                        },error = function(e){
                                
                        })
                        
                        
                        # para_judge_large = (nls.res$par > 100)
                        # t = length(which(para_judge_large == FALSE))
                        # if(t!= model$nParameters){
                        #         cat("judge large number\n")
                        #         next
                        # }
                        
                        
                        
                        if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)
                        
                        flag = model$flag * nls.res$par[2]
                        
                        if(flag<0){
                                nls.res$residual = 1000000
                                next
                        } 
                        
                        
                        
                        if(nls.res$residual < slmres$residual ){
                                slmres$residual  = nls.res$residual
                                slmres$par = nls.res$par
                        }
                        # if(slmres$residual < error)
                        #         break
                }
        }
        
        
        
        return (slmres)
}


##########################################################################
#Computing each formula's RMSE 
############################################################################




# fit all models, return best result
findBestFitting <- function(s,xData, yData,models,iter) {
        
        
        result <- NULL
        result$residual <- 1000000
        result$par <- NULL
        i = 1
        for(model in models) {
                model$iter <- iter[i] 
                singleRes <- NULL
                tempRes <- NULL
                tempRes <- nlsSolverSingleModel(model, xData, yData)
                singleRes <- model
                result <- model
                singleRes$residual = tempRes$residual
                singleRes$par = tempRes$par
                
                result <- singleRes
                i = i + 1
                
        }
        #  }
        return (result)
}






# file.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/ts0r0b/symmetric/berlin52"
# data.matrix = getTheMatrixData_TspSuite(file.path)
# head(data.matrix)
# data.matrix = data.matrix[order(data.matrix[,1]),]
# #data.matrix = data.matrix[sample.index,]
# fixxData = data.matrix[,1]
# fixyData = data.matrix[,3]
# xData <- fixxData
# yData <- fixyData
# xpara <- getInitialParameters(gompertzModelpositive, xData, yData)
# nls.res <- nlsSolverSingleModel(gompertzModelpositive,xData,yData)
# nls.res
# gompertzModelpositive$plotFunction(data.matrix,nls.res$par)
# xyRMSE(gompertzModelpositive$formula,xData,yData,nls.res$par)
# 
# count_error <- f_xyRMSE(gompertzModelpositive$formula,xData,yData,nls.res$par)
# sum (count_error*count_error)/length(xData)
# 
# 
# par <- c(-1.386003296,-1.746705991,1.630624083,-2.27735426)
# gompertzModelpositive$plotFunction(data.matrix,par)
# xyRMSE(gompertzModelpositive$formula,xData,yData,par)
# count_error <- f_xyRMSE(gompertzModelpositive$formula,xData,yData,par)
# sum (count_error*count_error)/length(xData)

#  file.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/maxSat/results/mFlipHCrs/uf250-10/"
#  data.matrix = getMatrixData(file.path)

# data.matrix = data.matrix[order(data.matrix[,1]),]
# fixxData = data.matrix[,1]
# fixyData = data.matrix[,3]
# plot(fixxData,fixyData, log = "x")
# xData <- fixxData
# yData <- fixyData
# plot(xData,yData, log = "x")
# logisticModelpositive$initFunc(xData,yData)
#  xpara <- getInitialParameters(decayModelpositive, xData, yData)
#  nls.res <- nlsSolverSingleModel(decayModelpositive,xData,yData)
#  xyRMSE(decayModelpositive$formula,xData,yData,nls.res$par)
#  count_error <- f_xyRMSE(decayModelpositive$formula,xData,yData,nls.res$par)
#  sum (count_error*count_error)/length(xData)
#
#
 # par <- c(6.347745104,-10.94887688,-0.336019904,-1.312252899)
 # xyRMSE(decayModelpositive$formula,xData,yData,par)
 # count_error <- f_xyRMSE(decayModelpositive$formula,xData,yData,par)
 # sum (count_error*count_error)/length(xData)
#
#  par <- c(75,0,0,0)
#  xyRMSE(decayModelpositive$formula,xData,yData,par)
#  count_error <- f_xyRMSE(decayModelpositive$formula,xData,yData,par)
#  sum(count_error*count_error)/length(xData)


