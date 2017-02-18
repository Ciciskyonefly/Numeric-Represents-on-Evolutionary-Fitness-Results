#Fuck!!!!!
remove(list = ls())
source("./base-funcs/models_var.R")
source("./base-funcs/maxsattsp_load_and_order_func.R")

list.model <- list(gompertzModelpositive)
# list_model = list(decayModelpositive,decayModelnegative,
#                   gompertzModelpositive,gompertzModelnegative,
#                   logisticModelpositive,logisticModelnegative,
#                   expLinearModelpositive,expLinearModelnegative
#                   )

file.path <- "./BenchMarking/modelresults/LM.tsp.pre/"
data.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"


PlotIntersect <- function(split.data,data.path){
        
        
        split.data <- split(split.data,split.data$instance_file)
        split.data <- split.data[c('ts0r0b/symmetric/att48')]
        data.names <- c('ts0r0b.att48')
       # pdf("different_data_volume.pdf")
       # par(mfrow = c(2,2))
        for(i in 1:length(names(split.data))){
                
                print(i)
                name.path = as.vector(split.data[[i]][1,2])
                path = paste(data.path,name.path,sep = "")
                data.matrix = getTheMatrixData_TspSuite(path)
                xData = data.matrix[,1]
                yData = data.matrix[,3]
                
                setEPS()
                names = paste(data.names[i],".eps",sep = "")
                postscript(names)

                
                plot(xData,yData,log = "x",pch = 3,col = "lightgray",xlab = "",ylab = "" ,main = name.path)
                grid(lwd = 1)
                
                xData = seq(1,ceiling(max(xData)),by = 0.1)
                temp.data <- split.data[[i]]
                legend.names <- c("10% data", "20% data", "30% data", "40% data", "50% data",
                                  "60% data", "70% data", "80% data", "90% data", "100% data")
                
                if(exists("legend.names")){
                        no.cbind = 1 
                }else{
                        legend.names <- NULL
                }
                
                for(j in 1:nrow(split.data[[i]])){
                        
                        model <- eval(parse(text= (as.vector(split.data[[i]][j,1]))))
                        
                        # if(grepl("expLinear",model$name)) next
                        # if(grepl("logisticModelnegative",model$name)) next
                        # if(grepl("expLinearModelnegative",model$name)) next
                        
                        if(!exists("no.cbind")){
                                legend.names <- cbind(legend.names,model$name)
                        }
                        
                        plot.data <- GetFormulaValue(model$formula,xData,yData,as.numeric(temp.data[j,c("a","b","c","d")]))
                        ylim = c(min(plot.data$fyData),max(plot.data$fyData) + 1)
                        
                        lines(plot.data[order(plot.data$xData),],col= rainbow(nrow(split.data[[i]]))[j],lwd = 2)
                        
                        
                        if(j == nrow(temp.data)){
                                legend("topright",legend=legend.names,
                                       col=rainbow(nrow(split.data[[i]])), lty= 1.8, cex=0.45)
                        }
                        
                }
                
                 dev.off()
                
                if(!exists("no.cbind")){
                        legend.names <- NULL  
                }
                
                for(j in 1:nrow(split.data[[i]])){
                        
                        model <- eval(parse(text= (as.vector(split.data[[i]][j,1]))))
                        
                        # if(grepl("expLinear",model$name)) next
                        # if(grepl("logisticModelnegative",model$name)) next
                        # if(grepl("expLinearModelnegative",model$name)) next
                        
                        if(!exists("no.cbind")){
                                legend.names <- cbind(legend.names,model$name)
                        }
                        
                        plot.data <- GetFormulaValue(model$formula,xData,yData,as.numeric(temp.data[j,c("a","b","c","d")]))
                        plot.data <- abs(plot.data)
                        # ylim = c(min(plot.data$fyData),max(plot.data$fyData) + 1)
                        
                        
                        if( j == 1){
                                plot(plot.data[order(plot.data$xData),],type = "l",log = "xy",xlab = "",bg = "gray",ylab = "",col= rainbow(nrow(split.data[[i]]))[j],lwd = 2 ,main = name.path)
                                grid()
                        } else {
                                
                                lines(plot.data[order(plot.data$xData),],col= rainbow(nrow(split.data[[i]]))[j],lwd = 2)
                        }
                        
                        if(j == nrow(temp.data)){
                                legend("bottomleft",legend=legend.names,
                                       col=rainbow(nrow(split.data[[i]])), lty= 1.8, cex=0.45)
                        }
                        
                }
                
                
        }
        
        dev.off()
}

GetFormulaValue <- function(formula, xData, yData, ppres){
        # Compute the residual of the model fitting value and the original yData value. 
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

AssembleData <- function(file.path, list.model ){
        
        
        for(j in 1:length(list.model)){
                
                merge.data <- NULL
                
                for(i in 1:10){
                        
                        inner.file <- paste(i*10,"percentleft", sep = "")
                        cat(inner.file,"\n")
                        list.name = list.files(paste(file.path, inner.file, "/" ,sep = ""))
                        
                        csv.list= grep(".csv",list.name)
                        if(length(csv.list) != 0){
                                list.name = list.name[csv.list] 
                        }else{
                                cat("no useful data.")
                                break
                        }
                        
                        data.name <- list.name[grep(unlist(list.model[j])$name,list.name)]
                        
                        temp.data <- read.csv(paste(file.path, inner.file, "/", data.name , sep = ""))
                        
                        merge.data <- if(is.null(merge.data)){
                                temp.data
                        } else {
                                rbind(merge.data,temp.data) 
                        }
                        
                }
                
                
                PlotIntersect(split.data =  merge.data, data.path  = data.path)
                print(merge.data)
       }
        
        
}


AssembleData(file.path, list.model)






