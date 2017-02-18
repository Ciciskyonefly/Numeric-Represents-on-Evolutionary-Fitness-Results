# 
# for(i in 1:length(uni.algorithm)){
#   algorithmMatrix = data.matrix[grepl(uni.algorithm[i],data.matrix[,"instance_file"]),]
#   resTable = generateTableMatrix(algorithmMatrix)
#   resPath = paste("../R Code/BenchMarking/OPResults/bbob_Analysing/","linearModel_",uni.algorithm[i],".csv",sep="")
#   write.csv(resTable,file = resPath)
# }
# 
# 
# 
# 
# datafileMatrix = data.matrix[grepl(unidatafile[1],data.matrix[,"instance_file"]),]
# ordermtable = data.matrix[order(data.matrix[,1]),]
# head(ordermtable)
# exponentialtable = ordermtable[grep("exponentialModel",ordermtable[,1]),]
# head(exponentialtable)
# colnames(data.matrix)
# write.csv(exponentialtable,"../R Code/BenchMarking/OPResults/bbobResult/exponentialModel_algorithm_para_relationship.csv")
# 

{"r:main-plug6-bbob data_f's conditioning"}
pdfname6 = "../R Code/BenchMarking/OPResults/bbob_Analysing/new-test_decayModel-condition-algorithm-data_f_BOBB.pdf"
pdf(pdfname6)
options(scipen = 200)
algorithmMatrix = NULL
par(mfrow = c(3,ncol(data.matrix)-2))
condition.matrix <- data_f.condition()
for(i in 1:length(uni.algorithm)){
  pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
  algorithm.matrix = data.matrix[grepl(pattern.ALG,data.matrix[,"instance_file"]),]
  if(is.null(algorithm.matrix)) next
  if(!is.matrix(algorithm.matrix)) 
    algorithm.matrix = t(as.matrix(algorithm.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
  
  for(j in 1:length(condition.matrix[,1])){
    patterString = paste(condition.matrix[j,1],"/",sep = "")
    datafile.matrix  = algorithm.matrix[grepl(patterString,algorithm.matrix[,"instance_file"]),]
    
    if(is.null(datafile.matrix))
      next
    
    if(!is.matrix(datafile.matrix)){
      datafile.matrix = t(as.matrix(datafile.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
    }
    
    
    tempMatrix = matrix(0,ncol = ncol(data.matrix)-2,nrow = length(unidata.txt),byrow = TRUE)    
    for(k in 1:length(unidata.txt)){
      pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
      if(length(grep(pattern.DIM,datafile.matrix[,"instance_file"])) == 0)
        next
      else
        tempMatrix[k,] = datafile.matrix[grep(pattern.DIM,datafile.matrix[,"instance_file"]),c(3:ncol(data.matrix))]
    }
    
    tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidata.txt),byrow = FALSE)
    if(ncol(tempMatrix) == 4){
      colnames(tempMatrix) = c("a","b","c","d")
      model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
    }else if(ncol(tempMatrix) == 3){
      colnames(tempMatrix) = c("a","b","c")
      model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
    }
    
    for(pp in 1:ncol(tempMatrix)){
      
      x <- c(2,3,5,10,20,40)
      y = tempMatrix[,pp]
      plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
      unidata.aixs = paste("DIM",c(1:40),sep = "")
      axis(1,at= x,labels = unidata.aixs[x],las = 2)
      axis(2)
      titleString = paste(uni.algorithm[i],"_",condition.matrix[j,1],"_",as.numeric(condition.matrix[j,2]),"_",colnames(tempMatrix)[pp],sep = "")
      title(main = titleString,col.main = "RED" )
      box()
      grid()
    }
  }
}

dev.off()


# 
# {"r:main-plug3-same data_f under different algorithm"}
# pdfname1 = "../R Code/BenchMarking/OPResults/bbob_Analysing/test_decayModel_data_f-algorithm-DIM_BBOB.pdf"
# pdf(pdfname1)
# algorithmMatrix = NULL
# par(mfrow = c(3,ncol(data.matrix)-2))
# 
# 
# for(j in 1:length(unidatafile)){
#   
#  for(i in 1:length(uni.algorithm)){
#   pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
#   algorithm.matrix = data.matrix[grepl(pattern.ALG,data.matrix[,"instance_file"]),]
#   if(is.null(algorithm.matrix)) next
#   if(!is.matrix(algorithm.matrix)) 
#     algorithm.matrix = t(as.matrix(algorithm.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
#   
# 
#     patterString = paste(unidatafile[j],"/",sep = "")
#     datafile.matrix  = algorithm.matrix[grepl(patterString,algorithm.matrix[,"instance_file"]),]
#     
#     if(is.null(datafile.matrix))
#       next
#     
#     if(!is.matrix(datafile.matrix)){
#       datafile.matrix = t(as.matrix(datafile.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
#     }
#     
#     
#     tempMatrix = matrix(0,ncol = ncol(data.matrix)-2,nrow = length(unidata.txt),byrow = TRUE)    
#     for(k in 1:length(unidata.txt)){
#       pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
#       if(length(grep(pattern.DIM,datafile.matrix[,"instance_file"])) == 0)
#         next
#       else
#         tempMatrix[k,] = datafile.matrix[grep(pattern.DIM,datafile.matrix[,"instance_file"]),c(3:ncol(data.matrix))]
#     }
#     
#     tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidata.txt),byrow = FALSE)
#    
#     if(ncol(tempMatrix) == 4){
#       colnames(tempMatrix) = c("a","b","c","d")
#       model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
#     }else if(ncol(tempMatrix) == 3){
#       colnames(tempMatrix) = c("a","b","c")
#       model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
#     }
#     
#     for(k in 1:ncol(tempMatrix)){
#       
#       x <- c(2,3,5,10,20,40)
#       y = tempMatrix[,pp]
#       plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
#       unidata.aixs = paste("DIM",c(1:40),sep = "")
#       axis(1,at= x,labels = unidata.aixs[x],las = 2)
#       axis(2)
#       titleString = paste(uni.algorithm[i],"_",unidatafile[j],"_",colnames(tempMatrix)[k],sep = "")
#       title(main = titleString,col.main = "RED" )
#       box()
#       grid()
#     }
#  }
#   
# }
# 
# dev.off()


{"r:rearrange.pdf-3"}
pdfname4 = "../R Code/BenchMarking/OPResults/bbob_Analysing/test_decayModel_DIM-data_f-algorithm_BBOB.pdf"
pdf(pdfname4)
algorithmMatrix = NULL
par(mfrow = c(3,ncol(data.matrix)-2))

for(k in 1:length(unidata.txt)){
  for(j in 1:length(unidatafile)){
    
    pattern.DATA = paste(unidatafile[j],"/",sep = "")
    DATA.tempMatrix = data.matrix[grep(pattern.DATA,data.matrix[,"instance_file"]),]
    
    pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
    if(length(grep(pattern.DIM,DATA.tempMatrix[,"instance_file"])) == 0)
      next
    DIM.tempMatrix = DATA.tempMatrix[grep(pattern.DIM,DATA.tempMatrix[,"instance_file"]),]
    if(!is.matrix(DIM.tempMatrix))
      DIM.tempMatrix = t(as.matrix(DIM.tempMatrix))
    
    tempMatrix = matrix(0,ncol = ncol(data.matrix) - 2,nrow = length(uni.algorithm),byrow = TRUE)
    
    for(i in 1:length(uni.algorithm)){
      pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
      if(length(grep(pattern.ALG,DIM.tempMatrix[,"instance_file"])) == 0)
        next
      else
        tempMatrix[i,] = DIM.tempMatrix[grep(pattern.ALG,DIM.tempMatrix[,"instance_file"]),c(3:ncol(data.matrix))]
      
    }
    
    tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(uni.algorithm),byrow = FALSE)
    
    
    
    if(ncol(tempMatrix) == 4){
      colnames(tempMatrix) = c("a","b","c","d")
      model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
    }else if(ncol(tempMatrix) == 3){
      colnames(tempMatrix) = c("a","b","c")
      model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
    }
    
    for(pp in 1:ncol(tempMatrix)){
      
      x = c(1:nrow(tempMatrix))
      y = tempMatrix[,pp]
      plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
      axis(1,1:nrow(tempMatrix),uni.algorithm,las = 2)
      axis(2)
      titleString = paste(unidata.txt[k],"-",unidatafile[j],"-",colnames(tempMatrix)[pp],sep = "")
      title(main = titleString,col.main = "RED" )
      box()
      grid()
    }
  }
}

dev.off()














{"r:main-plug1-every data_f's DIM para change"}
pdfname1 = "../R Code/BenchMarking/OPResults/bbob_Analysing/test_decayModel_algorithm-data_f-DIM_BOBB.pdf"
pdf(pdfname1)
algorithmMatrix = NULL
par(mfrow = c(3,ncol(data.matrix)-2))

for(i in 1:length(uni.algorithm)){
  pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
  algorithm.matrix = data.matrix[grepl(pattern.ALG,data.matrix[,"instance_file"]),]
  if(is.null(algorithm.matrix)) next
  if(!is.matrix(algorithm.matrix)) 
    algorithm.matrix = t(as.matrix(algorithm.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
  
  for(j in 1:length(unidatafile)){
    patterString = paste(unidatafile[j],"/",sep = "")
    datafile.matrix  = algorithm.matrix[grepl(patterString,algorithm.matrix[,"instance_file"]),]
    
    if(is.null(datafile.matrix))
      next
    
    if(!is.matrix(datafile.matrix)){
      datafile.matrix = t(as.matrix(datafile.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
    }
    
    
    tempMatrix = matrix(0,ncol = ncol(data.matrix)-2,nrow = length(unidata.txt),byrow = TRUE)    
    for(k in 1:length(unidata.txt)){
      pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
      if(length(grep(pattern.DIM,datafile.matrix[,"instance_file"])) == 0)
        next
      else
        tempMatrix[k,] = datafile.matrix[grep(pattern.DIM,datafile.matrix[,"instance_file"]),c(3:ncol(data.matrix))]
    }
    
    tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidata.txt),byrow = FALSE)
    if(ncol(tempMatrix) == 4){
      colnames(tempMatrix) = c("a","b","c","d")
      model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
    }else if(ncol(tempMatrix) == 3){
      colnames(tempMatrix) = c("a","b","c")
      model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
    }
    
    for(pp in 1:ncol(tempMatrix)){
      
      x <- c(2,3,5,10,20,40)
      y = tempMatrix[,pp]
      plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
      unidata.aixs = paste("DIM",c(1:40),sep = "")
      axis(1,at= x,labels = unidata.aixs[x],las = 2)
      axis(2)
      titleString = paste(uni.algorithm[i],"_",unidatafile[j],"_",colnames(tempMatrix)[pp],sep = "")
      title(main = titleString,col.main = "RED" )
      box()
      grid()
    }
  }
}

dev.off()




{"r:main-7-bbob data_f's conditiong"}
pdfname7 = "../R Code/BenchMarking/OPResults/bbob_Analysing/new-test_decayModel_condition-algorithm-data_f_BOBB.pdf"
pdf(pdfname7)
options(scipen = 200)
algorithmMatrix = NULL
par(mfrow = c(3,ncol(data.matrix)-2))
unidatafile.success <- c("data_f1","data_f2","data_f5","data_f8","data_f9")
for(i in 1:length(uni.algorithm)){
  pattern.ALG = paste(uni.algorithm[i],"/",sep = "")
  algorithm.matrix = data.matrix[grepl(pattern.ALG,data.matrix[,"instance_file"]),]
  if(is.null(algorithm.matrix)) next
  if(!is.matrix(algorithm.matrix)) 
    algorithm.matrix = t(as.matrix(algorithm.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
  
  for(j in 1:length(unidatafile.success)){
    patterString = paste(unidatafile.success[j],"/",sep = "")
    datafile.matrix  = algorithm.matrix[grepl(patterString,algorithm.matrix[,"instance_file"]),]
    
    if(is.null(datafile.matrix))
      next
    
    if(!is.matrix(datafile.matrix)){
      datafile.matrix = t(as.matrix(datafile.matrix,nrow = 1,ncol = ncol(data.matrix),byrow = TRUE))
    }
    
    
    tempMatrix = matrix(0,ncol = ncol(data.matrix)-2,nrow = length(unidata.txt),byrow = TRUE)    
    for(k in 1:length(unidata.txt)){
      pattern.DIM = paste("_",unidata.txt[k],"/",sep = "")
      if(length(grep(pattern.DIM,datafile.matrix[,"instance_file"])) == 0)
        next
      else
        tempMatrix[k,] = datafile.matrix[grep(pattern.DIM,datafile.matrix[,"instance_file"]),c(3:ncol(data.matrix))]
    }
    
    tempMatrix = matrix(as.numeric(tempMatrix),nrow = length(unidata.txt),byrow = FALSE)
    if(ncol(tempMatrix) == 4){
      colnames(tempMatrix) = c("a","b","c","d")
      model.ylim = matrix(c(-1,1,-2,-1,1,2,0,4),ncol = 2,byrow = TRUE)
    }else if(ncol(tempMatrix) == 3){
      colnames(tempMatrix) = c("a","b","c")
      model.ylim = matrix(c(0,8,-2,15,-35,2),ncol = 2,byrow = TRUE)
    }
    
    for(pp in 1:ncol(tempMatrix)){
      x <- c(2,3,5,10,20,40)
      y = tempMatrix[,pp]
      plot(x,y,pch = 3,axes = FALSE,xlab = "FEs_Algorithm",ylab = "F",col = "BLUE")
      unidata.aixs = paste("DIM",c(1:40),sep = "")
      axis(1,at= x,labels = unidata.aixs[x],las = 2)
      axis(2)
      titleString = paste(uni.algorithm[i],"_",unidatafile.success[j],"_",colnames(tempMatrix)[pp],sep = "")
      title(main = titleString,col.main = "RED" )
      box()
      grid()
    }
  }
}

dev.off()
