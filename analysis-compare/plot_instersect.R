rm(list = ls())

source("./models_var.R")
source("./data_preprocessing/readdata.R")


load<-function(file.path = "./modelresults/tspResult/250_gompertzModelpositives_model.csv"){
        data.table = read.csv(file.path,header = TRUE)
        pre.length = nrow(data.table)
        colnames(data.table)
        #if(length(which(data.table[,"residuals"] > 1000)) > 0)
        #        data.table = data.table[-which(data.table[,"residuals"] > 1000),]
        #data.table <- data.table[,-ncol(data.table)]
        after.length = nrow(data.table)
        print("Correct rate:")
        print(after.length/pre.length *100)
        #     data.matrix = as.matrix(data.table)
        return(data.table)
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

AssembleData <- function(file.path,size = 250 ){
        
        list.name = list.files(file.path)
        csv.list= grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }
        
        
        size.list = grep(size,list.name)
        
        if(length(size.list)==0){
                cat("no useful data.")
                break
        }else
                list.name = list.name[size.list]
        
       print(list.name) 
        
        merge.data <- NULL
        
        for(i in 1:length(list.name)){
                
                data.path = paste(file.path,list.name[i],sep = "")
                #      cat(list.name[i],"\n")
                cat(data.path)
                para.matrix <- load(data.path)
                
                merge.data <- rbind(merge.data,para.matrix)
        }
        
        return (merge.data)
        
}


file.path = "./modelresults/LM.tsp/1_y/"
data.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"
size = 10
#data.path = file.path
merge.data <- AssembleData(file.path,size = size)

split.data <- split(merge.data,merge.data$instance_file)

split.data <- split.data[c('ts1e7r0f/symmetric/gr21','ts1e7r0f/symmetric/dantzig42','ts1e7r0f/symmetric/att48')]
data.names <- c('ts1e7r0fgr21','ts1e7r0fdantzig42','ts1e7r0fatt48')

# split.data <- split.data[c('ts0r0b/symmetric/brazil58','ts10r10b/symmetric/brazil58','ts1e7r0b/symmetric/brazil58',
#                            'ts0r0f/symmetric/brazil58','ts10r10f/symmetric/brazil58','ts1e7r0f/symmetric/brazil58')]
# 
# legend.names <- c("no-up-bound","up-bound")

# data.names <- c('ts0r0b_brazil58','ts10r10b_brazil58','ts1e7r0b_brazil58',
# 'ts0r0f_brazil58','ts10r10f_brazil58','ts1e7r0f_brazil58')

PlotInterset <- function(split.data,data.path){
        # pdf("summary_plot_remove_bad_fits.pdf")
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

                
                plot(xData,yData,log = "x",pch = 3,col = "lightgray",main = name.path)
                grid(lwd = 1)
                
                xData = seq(1,ceiling(max(xData)),by = 0.1)
                temp.data <- split.data[[i]]
                
                
                if(exists("legend.names")){
                        no.cbind = 1 
                }else{
                        legend.names <- NULL
                }
                
                for(j in 1:nrow(split.data[[i]])){
                        
                        model <- eval(parse(text= (as.vector(split.data[[i]][j,1]))))
                        
                        if(grepl("expLinear",model$name)) next
                        if(grepl("logisticModelnegative",model$name)) next
                        if(grepl("expLinearModelnegative",model$name)) next
                        # 
                        if(!exists("no.cbind")){
                                legend.names <- cbind(legend.names,model$name)
                        }
                        
                        plot.data <- GetFormulaValue(model$formula,xData,yData,as.numeric(temp.data[j,c("a","b","c","d")]))
                        ylim = c(min(plot.data$fyData),max(plot.data$fyData) + 1)
                        
                        lines(plot.data[order(plot.data$xData),],col= j,lwd = 2)
                        
                        
                        if(j == nrow(temp.data)){
                                legend("topright",legend=legend.names,
                                       col=c(1:nrow(temp.data)), lty= 1,lwd = 2, cex=0.45)
                        }
                        
                }
                
                dev.off()
                
                if(!exists("no.cbind")){
                        legend.names <- NULL  
                }
                
                for(j in 1:nrow(split.data[[i]])){
                        
                        model <- eval(parse(text= (as.vector(split.data[[i]][j,1]))))
                        if(grepl("expLinear",model$name)) next
                        if(grepl("logisticModelnegative",model$name)) next
                        if(grepl("expLinearModelnegative",model$name)) next
                        
                        if(!exists("no.cbind")){
                                legend.names <- cbind(legend.names,model$name)
                        }
                        
                        plot.data <- GetFormulaValue(model$formula,xData,yData,as.numeric(temp.data[j,c("a","b","c","d")]))
                        plot.data <- abs(plot.data)
                        # ylim = c(min(plot.data$fyData),max(plot.data$fyData) + 1)
                        
                        
                        if( j == 1){
                                plot(plot.data[order(plot.data$xData),],type = "l",log = "xy",xlab = "",bg = "gray",ylab = "",col = j,lwd = 2 ,main = name.path)
                                grid()
                        } else {
                                
                                lines(plot.data[order(plot.data$xData),],col= j,lwd = 2)
                        }
                        
                        if(j == nrow(temp.data)){
                                legend("bottomleft",legend=legend.names,
                                       col=c(1:nrow(temp.data)), lty= 1.8, cex=0.45)
                        }
                        
                }
                
                
                #dev.off()
        }
        
        # dev.off()
}


PlotInterset(split.data = split.data, data.path = data.path)
