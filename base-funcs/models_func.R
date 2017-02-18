
getInitialParameters <- function(model, xData, yData) {
        # Provide initial parameters for model fitting.
        #
        # Args:
        #   model: The model we try to fit.
        #   xData: A vector of runtime in an instances.
        #   yData: A vector of function evaluations in an instances.
        #
        # Returns:
        #   A vector of initial parameters that can be used in Gauss-Newton and LM numeric fitting methods.
        
        xDataLength = length(xData)
        partition = xDataLength/(2*model$nParameters)
        partx <- NULL
        party <- NULL
        for(i in 1:model$nParameters){
                partx[i] = mean(xData[(2*(i-1)*partition+1):((2*i-1)*partition)])
                party[i] = mean(yData[(2*(i-1)*partition+1):((2*i-1)*partition)])
        }
        xData = partx
        yData = party
        
        tempFunction <- function(x) {
                F <- rep(NA, model$nParameters)
                for(i in 1:model$nParameters) {
                        F[i] <- model$modelFunction(modelParams=x, xData=xData[i]) - yData[i]
                }
                F
        }
        theta = NULL
        start.init <- model$initFunc(xData,yData)
        tryCatch({
                theta = dfsane(par= start.init, fn=tempFunction,control = list(maxit = 2000,trace = FALSE))
        },error = function(e){
                theta$par <- rep(0,model$nParameters)
        })
        if(is.null(theta$par))
                theta$par <- rep(0,model$nParameters)
        
        
        return (theta$par)
        
}


xyRMSE <- function(y, pred.y) {
        # Use for calculating residuals.
        # 
        square.residuals <- (y - pred.y) * (y - pred.y)
        
        if( which(y == 0)%>% length() != 0 )
                y[which(y == 0)] = min(abs(y[-which(y == 0)]))/2
        
        eva.residuals <- sum(square.residuals/(y - min(y) + 0.1))/length(y)
        return (eva.residuals)
}




#Can be the implementation of LM.R temp$res caculation method 
GetFormulaValue <- function(formula, xData, ppres){
        # Compute the fitting error of the model . 
        #
        # Args:
        #   formula: A nonlinear model formula.   
        #   xData: A vector of data used to compute the formula value. 
        #   yData: A vector of original function evaluates data.
        #   ppres: A vector of parameters that will used in formula.
        #
        # Returns:
        #   A data frame that contain xData and the value caculated by formula. 
        
        n = length(xData)
        lengthPara = length(ppres)
        if(lengthPara  == 3){
                a = rep(ppres[1], n)
                b = rep(ppres[2], n)
                c = rep(ppres[3], n)
        }else if(lengthPara  == 4){
                a = rep(ppres[1], n)
                b = rep(ppres[2], n)
                c = rep(ppres[3], n)
                d = rep(ppres[4], n)
        }
        x = xData
        
        # Converse "formula" form to the form that can be use a,b,c,x directly computing. 
        Fformula = Formula(formula)
        rFformula = attributes(Fformula)
        fyData =  eval(rFformula$rhs[[1]]) 
        return (data.frame(xData,fyData))
}



