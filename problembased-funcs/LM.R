source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
source("./base-funcs/readdata_func.R")
library(ggplot2)
library(dplyr)
nlsSolverSingleModel <- function(s, model, xData, yData){
        
        
        fixxData <- s$x
        fixyData <- s$y
        
        slmres = NULL 
        slmres$residual = 10000000
        slmres$par = model$initFunc(xData,yData)
        
        nls.res = NULL
        nls.res$residual = 10000000
        nls.res$par <- model$initFunc(xData,yData)
        
        temp.res = NULL
        temp.res$par <- NULL
        temp.res$residual <- +Inf
        
        x = xData
        y = yData  
        
        
        y <- abs(y)
        if(which(y == 0) %>% length() != 0){
            y[which(y == 0)] = min(abs(y[-which(y == 0)]))/2
        }
        
        # y <- abs(y)
        # if(min(y) <= 0){
        #         yy= y
        #         yy = yy[-which(yy <= 0)]
        #         y[which(y == 0)] = (min(yy)/2) 
        # }
        # 
        
        set.seed(59)

        for(pp in 1:model$iter){

                initialPara <- getInitialParameters(model, xData, yData)
                nls.res$par <- initialPara
                nls.res$residual <- 10000000
                tryCatch({
                        startlist = initialPara
                        datalist  = list(y=y,x=x)
                        fit = nlsLM(model$formula,data =datalist,start = model$nlsstart(startlist),
                                    control = list(maxiter = 100),
                                    weights = (1/abs(y))
                        #            weights = (1/abs(y- min(y) + 0.1))
                                    )
                        nls.res$par = as.vector(coef(fit))
                        pred_y <- predict(fit, data.frame(fixxData)) %>% as.vector()
                        nls.res$residual = xyRMSE(fixyData, pred_y)
                },error = function(e){
                })
                
                if(nls.res$residual < temp.res$residual ){
                        temp.res$residual  = nls.res$residual
                        temp.res$par = nls.res$par
                }

                if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)

                flag = model$flag * nls.res$par[2]

                if(flag <= 0) next
                
                if(nls.res$par[2] > 50 * max(yData) )
                        next
                        

                if(nls.res$residual < slmres$residual ){
                        slmres$residual  = nls.res$residual
                        slmres$par = nls.res$par
                }
                # if(slmres$residual < error)
                #         break
        }

        if(slmres$residual >= 10000000){
                
                for(pp in 1:model$iter){
                        
                        initialPara <- model$initFunc(xData,yData)
                        nls.res$par =  initialPara
                        
                        nls.res$residual <- 10000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,
                                            data =datalist,
                                            start = model$nlsstart(startlist),
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf),
                                            control = list(maxiter = 100),
                                            weights = (1/abs(y))
                                       #     weights = (1/y)
                                            )
                                nls.res$par = as.vector(coef(fit))
                              #  cat("nls.res$par: ", nls.res$par, "\n")
                                pred_y <- predict(fit, data.frame(fixxData)) %>% as.vector()
                                nls.res$residual = xyRMSE(fixyData, pred_y)
                                
                        },error = function(e){
                        })
                        
                        if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)
                        
                        if(nls.res$residual < temp.res$residual ){ #update temp.res
                                temp.res$residual  = nls.res$residual
                                temp.res$par = nls.res$par
                        }
                        
                        #if not satisfy for up-bound limits setting break
                        if(model$flag * nls.res$par[2]<=0) next
                        
                        if(nls.res$residual < slmres$residual ){ #update slmres.res
                                slmres$residual  = nls.res$residual
                                slmres$par = nls.res$par
                        }
                }
                
                
                
        }
        if(slmres$residual >= 10000000){
                print(2)

                for(pp in 1:model$iter){

                        initialPara <- model$initFunc(xData,yData)
                        nls.res$par =  initialPara
                        nls.res$residual <- 10000000
                        tryCatch({
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,
                                            data =datalist,
                                            start = model$nlsstart(startlist),
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf),
                                            control = list(maxiter = 100),
                                            weights = (1/abs(y))
                                #            weights = (1/(y-min(y) + 0.1))
                                )
                                
                                nls.res$par = as.vector(coef(fit))
                                library(dplyr)
                                pred_y <- predict(fit, data.frame(fixxData)) %>% as.vector()
                                nls.res$residual = xyRMSE(fixyData, pred_y)
                                
                        },error = function(e){
                        })

                        if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)

                       if(model$flag * nls.res$par[2]<=0) next



                        if(nls.res$residual < slmres$residual ){ #update slmres.res
                                slmres$residual  = nls.res$residual
                                slmres$par = nls.res$par
                        }
                }
                
                
                
        }

        
        if(slmres$residual >= 10000000){
                
                print(3)
                for(pp in 1:model$iter){
                        
                        initialPara <- rnorm(model$nParameters)
                        nls.res$par <- initialPara
                        nls.res$residual <- 10000000
                        tryCatch({
                                #  print(1)
                                startlist = initialPara
                                datalist  = list(y=y,x=x)
                                fit = nlsLM(model$formula,data =datalist,start = model$nlsstart(startlist), 
                                            upper = c(+Inf,30*max(yData),+Inf,+Inf), 
                                            control = list(maxiter = 100), 
                                            weights = (1/abs(y))
                                    #        weights = (1/(y-min(y) + 0.1))
                                            )
                                nls.res$par = as.vector(coef(fit))
                                pred_y <- predict(fit, data.frame(fixxData)) %>% as.vector()
                                nls.res$residual = xyRMSE(fixyData, pred_y)
                             
                        },error = function(e){
                        })
                        
                
                        
                        if(is.null(nls.res$par))  nls.res$par <- rep(0,model$nParameters)
                        
                        if(model$flag * nls.res$par[2]<=0) next
                        
                        if(nls.res$residual < slmres$residual ){ #update slmres
                                slmres$residual  = nls.res$residual
                                slmres$par = nls.res$par
                        }
                }
        }
        
        # all fitting try fails, choose the old best parameters, but residual has to be 1000000
        if(slmres$residual >= 10000000){ 
                slmres$residual  = temp.res$residual
                slmres$par = temp.res$par
        }
        
        return (slmres)
}




# # fit all models, return best result
# findBestFitting <- function(s, xData, yData, models, iter) {
#         
#         
#         result <- NULL
#         result$residual <- 1000000
#         result$par <- NULL
#         i = 1
#         for(model in models) {
#                 model$iter <- iter[i] 
#                 singleRes <- NULL
#                 tempRes <- NULL
#                 tempRes <- nlsSolverSingleModel(s,model, xData, yData)
#                 singleRes <- model
#                 result <- model
#                 singleRes$residual = tempRes$residual
#                 singleRes$par = tempRes$par
#                 
#                 result <- singleRes
#                 i = i + 1
#                 
#         }
#         #
#         return (result)
# }


# fit all models, return best result
findBestFitting <- function(s, xData, yData, models, iter) {
        #2016 12/31 fixed  
        
        
        result <- NULL
        result <- models[[1]]
        result$residual <- 1000000
        result$par <- c(0, 0, 0, 0)
        
        i = 1
        for(mod in 1:length(models)) {
                
                model <- models[[mod]] 
                model$iter <- iter[i] 
                temp.res <- nlsSolverSingleModel(s,model, xData, yData)
                
                single.res<- model
                single.res$residual = temp.res$residual
                single.res$par = temp.res$par
                if(single.res$residual < result$residual){
                        result <- single.res
                }
                
        }
        return (result)
}
