source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
library(ggplot2)
library(dplyr)
LMSolverSingleModel <- function(future.data, model, xData, yData){
        
        fix.xData <- future.data$x
        fix.yData <- future.data$y
        
        lm.res = NULL 
        lm.res$residual = 10000000
        lm.res$par = model$initFunc(xData,yData)
        
        #temp varible
        temp.res = NULL
        temp.res$residual = 10000000
        temp.res$par <- model$initFunc(xData,yData)
        
        #history best results 
        his.res = NULL
        his.res$par <- NULL
        his.res$residual <- 10000000
        
        x = xData
        y = yData  
        
        
        #set radom seed
        set.seed(59)

        for(it in 1:model$iter){
            
                # fitting_func <- function(model, initialPara, xData, yData, weights)
                # model
                # Weights Function <- func
                #
                #

                initialPara <- getInitialParameters(model, xData, yData)
                temp.res$par <- initialPara
                temp.res$residual <- 10000000
                tryCatch({
                        startlist = initialPara
                        datalist  = list(y = y,x = x)
                        fit = nlsLM(model$formula, data = datalist,
                                    start = model$nlsstart(startlist),
                                    control = list(maxiter = 100),
                                    weights = weights_funcs(y)
                                    )
                        temp.res$par = as.vector(coef(fit))
                        pred_y <- GetFormulaValue(model$formula, fix.xData, temp.res$par)[, 2]
                        temp.res$residual = xyRMSE(fix.yData, pred_y)
                },error = function(e){
                })
                
                
                # update history best results.
                # history best results probably not satisfy all constraints
                # if all our models fails, we choose history best results as back up final results.
                if(temp.res$residual < his.res$residual ){
                    his.res$residual  = temp.res$residual
                    his.res$par = temp.res$par
                }
                
                
                if(is.null(temp.res$par))  temp.res$par <- rep(0,model$nParameters)
                
                #parameters constraints
                flag = model$flag * temp.res$par[2]
                if(flag <= 0) next
                if(temp.res$par[2] > 50 * max(yData) )
                    next

                # updata final results.                
                if(temp.res$residual < lm.res$residual ){
                    lm.res$residual  = temp.res$residual
                    lm.res$par = temp.res$par
                }
                
            

        }

        # set up-bound
        if(lm.res$residual >= 10000000){
            
                for(it in 1:model$iter){
                        initialPara <- model$initFunc(xData,yData)
                        temp.res$par =  initialPara
                        temp.res$residual <- 10000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,
                                            data =datalist,
                                            start = model$nlsstart(startlist),
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf),
                                            control = list(maxiter = 100),
                                            weights = weights_funcs(y)
                                            )
                                temp.res$par = as.vector(coef(fit))
                                pred_y <- GetFormulaValue(model$formula, fix.xData, temp.res$par)[, 2]
                                temp.res$residual = xyRMSE(fix.yData, pred_y)
                                
                        },error = function(e){
                        })
                        
                        if(is.null(temp.res$par))  temp.res$par <- rep(0,model$nParameters)
                        
                        if(temp.res$residual < his.res$residual ){ #update his.res
                                his.res$residual  = temp.res$residual
                                his.res$par = temp.res$par
                        }
                        
                        #if not satisfy for up-bound limits setting break
                        if(model$flag * temp.res$par[2]<=0) next
                        
                        if(temp.res$residual < lm.res$residual ){ #update lm.res
                                lm.res$residual  = temp.res$residual
                                lm.res$par = temp.res$par
                        }
                }
                
                
                
        }
        if(lm.res$residual >= 10000000){
                print(2)
                for(it in 1:model$iter){
                        initialPara <- model$initFunc(xData,yData)
                        temp.res$par =  initialPara
                        temp.res$residual <- 10000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,
                                            data =datalist,
                                            start = model$nlsstart(startlist),
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf),
                                            control = list(maxiter = 100),
                                            weights = weights_funcs(y)
                                )
                                
                                temp.res$par = as.vector(coef(fit))
                                library(dplyr)
                                pred_y <- GetFormulaValue(model$formula, fix.xData, temp.res$par)[, 2]
                                temp.res$residual = xyRMSE(fix.yData, pred_y)
                                
                        },error = function(e){
                        })

                        if(is.null(temp.res$par))  temp.res$par <- rep(0,model$nParameters)

                       if(model$flag * temp.res$par[2]<=0) next
                        #update lm.res
                        if(temp.res$residual < lm.res$residual ){ 
                                lm.res$residual  = temp.res$residual
                                lm.res$par = temp.res$par
                        }
                }
        }

        
        if(lm.res$residual >= 10000000){
                print(3)
                for(it in 1:model$iter){
                        
                        initialPara <- rnorm(model$nParameters)
                        temp.res$par <- initialPara
                        temp.res$residual <- 10000000
                        tryCatch({
                                #  print(1)
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,data =datalist,start = model$nlsstart(startlist), 
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf), 
                                            control = list(maxiter = 100), 
                                            weights = weights_funcs(y)
                                            )
                                temp.res$par = as.vector(coef(fit))
                                pred_y <- GetFormulaValue(model$formula, fix.xData, temp.res$par)[, 2]
                                temp.res$residual = xyRMSE(fix.yData, pred_y)
                             
                        },error = function(e){
                        })
                        
                
                        
                        if(is.null(temp.res$par))  temp.res$par <- rep(0,model$nParameters)
                        
                        if(model$flag * temp.res$par[2]<=0) next
                        
                        if(temp.res$residual < lm.res$residual ){ #update lm.res
                                lm.res$residual  = temp.res$residual
                                lm.res$par = temp.res$par
                        }
                }
        }
        
        # all fitting try fails, choose the old best parameters, but residual has to be 1000000
        if(lm.res$residual >= 10000000){ 
                lm.res$residual  = his.res$residual
                lm.res$par = his.res$par
        }
        
        return (lm.res)
}



# fit all models, return best result
findBestFitting <- function(future.data, xData, yData, models, iter) {
        
       
        #set initial value
        result <- NULL
        result <- models[[1]]
        result$residual <- 1000000
        result$par <- c(0, 0, 0, 0)
        
        i = 1
        for(mod in 1:length(models)) {
                
                model <- models[[mod]] 
                model$iter <- iter[i] 
                temp.res <- LMSolverSingleModel(future.data, model, xData, yData)
                
                single.res<- model
                single.res$residual = temp.res$residual
                single.res$par = temp.res$par
                if(single.res$residual < result$residual){
                        result <- single.res
                }
                
        }
        return (result)
}
