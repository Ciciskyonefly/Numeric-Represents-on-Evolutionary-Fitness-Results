rm(list = ls())

source("./models_var.R")
source("./models_func.R")
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



#data.path = file.path

ComputeCorrectPredictModelResidual <- function(file.path,data.path){
        
        para.matrix <- load(file.path)
        model <- eval(parse(text= (as.vector(para.matrix[1,1]))))
        for(i in 1:nrow(para.matrix)){
                name.path <- para.matrix$instance_file[i]
                path = paste(data.path,name.path,sep = "")
                data.matrix = getTheMatrixData_TspSuite(path)
                xData = data.matrix[,1]
                yData = data.matrix[,3]
                para.matrix$residuals[i] <- xyRMSE(model$formula,xData,yData,as.numeric(para.matrix[i,c("a","b","c","d")]))
        }
        
        
        write.csv(para.matrix,file = file.path,row.names = FALSE)
}
        


           

correct_compute_residual <- function(file.path,data.path){
        list.name = list.files(file.path)
        csv.list= grep(".csv",list.name)
        if(length(csv.list) != 0){
                list.name = list.name[csv.list] 
        }else{
                cat("no useful data.")
                break
        }    
        for(i in 1:length(list.name)){
                temp.path = paste(file.path,list.name[i],sep = "")
                ComputeCorrectPredictModelResidual(temp.path,data.path)
                print(temp.path)
        }
}

file.path = "./BenchMarking/modelresults/LM.tsp.pre/50percentsremove/"
data.path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"

correct_compute_residual(file.path,data.path)
