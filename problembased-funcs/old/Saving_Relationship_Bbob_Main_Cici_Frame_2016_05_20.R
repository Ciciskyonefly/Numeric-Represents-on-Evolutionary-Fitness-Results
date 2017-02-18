#run sucess 
#use this procedure fitting bobb with all bbob model
#fix on this procedure! 2016/5/18
rm(list = ls())
library("BB")
#include RMSE
#library(qpcR)
library(Formula)
source("../R_Code/core_fitting_procedure.R")
getTheMatrixData_i <- function(filePath,i){
  #make use of regular expressions,filtrate the document suffix 
  a = list.files(filePath,pattern = ".*?\\.dat")
  #  for(i in 2:length(a)){
  dirname = paste(filePath,"/",a[i],sep="")
  con <- file(dirname, "r")
  line = readLines(con,n=1)
  merge.data = as.vector(unlist(strsplit(line,"\\|")))
  while( length(line) != 0 ) {
    line=readLines(con,n=1)
    if(grepl("%.*",line) == FALSE&&length(line) != 0 ){
      line = strsplit(line," ")
      line = as.vector(unlist(line))
      merge.data = rbind(merge.data,line,deparse.level = 0)
    }
  }
  close(con)
  
  #  }
  s = merge.data[,1:3]
  colnames(s) = s[1,]
  s = s[-1,]
  s=matrix(as.numeric(s),nrow=nrow(s))
  return (s)
}  
filePath = "/home/cici/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/bbob/"
list = list.files(filePath)
list = list[-grep("\\.",list)]
print.list =  list
list = paste(filePath,list,sep = "")
start.time = Sys.time()

csvname1 = "../R_Code/BenchMarking/OPResults/bbobResult/test_relationship_bbob.csv"
csvname.logistic = "../R_Code/BenchMarking/OPResults/bbobResult/test_logisticModel_bbob_10_14.csv"
csvname.decay = "../R_Code/BenchMarking/OPResults/bbobResult/test_decaypasitive_bbob_10_14.csv"
csvname.expLinear = "../R_Code/BenchMarking/OPResults/bbobResult/test_expLinearModel_bbob_10_14.csv"
csvname.gompertz = "../R_Code/BenchMarking/OPResults/bbobResult/test_gompertzModelpasitive_bbob.csv"


relationship = NULL
logParaCsv = NULL
decayParaCsv = NULL
linearParaCsv = NULL
gompertzParaCsv = NULL
pdfname = "../R_Code/BenchMarking/OPResults/bbobResult/plot_decaypasitive_10_14.pdf"
pdf(pdfname)
par(mfrow = c(2, 2))
count <- 0
for(s in 1:length(list)){

  count <- count + 1
  print(paste("Process:",count))
  a = list.files(list[s])
  #FILTRAT THE FILE
  datadoc = a[-grep("\\.",a)]
  logsp = NULL
  sp = NULL
  for(i in 1:length(datadoc)){
    setdirpath = paste(list[s],"/",datadoc[i],sep = "")
    print.setdirpath = paste(print.list[s],"/",datadoc[i],sep = "")
    bb = list.files(setdirpath,pattern = ".*?\\.dat")
    for(j in 1:length(bb)){
      bbname = unlist(strsplit(bb[j],"\\."))[1]
      print(bbname)
      algorithm_instances_name = paste(print.list[s],"/",datadoc[i],"/",bbname,sep = "")
      ss = getTheMatrixData_i(setdirpath,j)
      ss = ss[order(ss[,1]),]
      xData= ss[,1]
      yData = ss[,3]
      globalmodels <- list(decayModelpasitive)
   #   globalmodels <- list(expLinearModel,logisticModel,decayModelpasitive,decayModelpositive,gompertzModelpositive,gompertzModelpasitive)
      p = findBestFitting(ss,xData,yData,globalmodels)
      p$plotFunction(ss,p$par)
      p$par = round(p$par,4)
      pathname = unlist(strsplit(bbname,"_"))
      path = paste(pathname[2],"_",pathname[3],sep="")
      temp_relationship = cbind(p$name,algorithm_instances_name,p$residual,deparse.level = 0)
      temp_Para = cbind(algorithm_instances_name,p$name,t(p$par),p$residual,deparse.level = 0)
    #  print(temp_Para)
 #     relationship = rbind(relationship,temp_relationship,deparse.level = 0)
      if(grepl("expLinearModel",p$name)){
        mainString =  paste(print.list[s],"_",path,"_","expLinearModel")
        linearParaCsv = rbind(linearParaCsv,temp_Para,deparse.level = 0)
        title(main = mainString,col.main = "RED")
        title(sub = "expLinearModel",col.sub = "RED")
        
        
      }
      if(grepl("logisticModel",p$name)){
        logParaCsv = rbind(logParaCsv,temp_Para,deparse.level = 0)
        mainString =  paste(print.list[s],"_",path,"_","logisticModel")
        title(main = mainString,col.main = "BLUE")
        title(sub = "bbob_logisticModel",col.sub = "BLUE")
        
      }
      if(grepl("decay",p$name)){
        decayParaCsv = rbind(decayParaCsv,temp_Para,deparse.level = 0)
        mainString =  paste(print.list[s],"_",path,"_","decayModel")
        title(main = mainString,col.main = "PURPLE")
        title(sub = "decayModel",col.sub = "PURPLE")
        
      }
      
      if(grepl("gompertzModel",p$name)){
        gompertzParaCsv = rbind(gompertzParaCsv,temp_Para,deparse.level = 0)
        mainString =  paste(print.list[s],"_",path,"_","gompertzModel")
        title(main = mainString,col.main = "ORANGE")
        title(sub = "bbob_gompertzModel",col.sub = "ORANGE")
        
      }
      
    }#for
    
  
  }#for
  
}
dev.off()

###########
if(!is.null(relationship)){
        colnames(relationship) <- c("model","instance_file","residuals")
        write.csv(relationship,csvname1,row.names = FALSE,col.names = TRUE)
}

if(!is.null(logParaCsv)){
        colnames(logParaCsv) <- c("instance_file","model","a","b","c","residuals")
        write.csv(logParaCsv,csvname.logistic,row.names = FALSE,col.names = TRUE)
}

if(!is.null(decayParaCsv)){
        colnames(decayParaCsv) <- c("instance_file","model","a","b","c","d","residuals")
        write.csv(decayParaCsv,csvname.decay,row.names = FALSE,col.names = TRUE)
}

if(!is.null(linearParaCsv)){
        colnames(linearParaCsv) <- c("instance_file","model","a","b","c","d","residuals")
        write.csv(linearParaCsv,csvname.expLinear,row.names = FALSE)
}
        
        

if(!is.null(gompertzParaCsv)){
        colnames(gompertzParaCsv) <- c("instance_file","model","a","b","c","d","residuals")
        write.csv(gompertzParaCsv,csvname.gompertz,row.names = FALSE )
}

end.time = Sys.time()
consumetime = end.time - start.time
print (consumetime)