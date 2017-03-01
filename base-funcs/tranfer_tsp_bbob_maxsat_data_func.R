rm(list = ls())
source("./base-funcs/readdata_func.R")
library(dplyr)

transferMaxsat <- function(){
        
        library(dplyr)
        file_path = "../Benchmarking/optimizationBenchmarkingDocu-master/examples/maxSat/results/"
        save_path = "./rawdata/maxsat/"
        TotalPath = file_path
        respath = save_path
        firstFileList = TotalPath %>% list.files()
        firstFileList = firstFileList[-grep("\\.",firstFileList)]
        count.process = 0
        jFirstLayer = 1
        for(jFirstLayer in 1:length(firstFileList)){
                firstFilePath = paste(TotalPath,firstFileList[jFirstLayer],"/",sep = "")
                filename = paste(firstFileList[jFirstLayer],sep = "")
                a = list.files(firstFilePath)
                i = 1
                for(i in 1:length(a)){
                        save.name = paste(filename,"_",a[i],sep = "")
                        if(grepl("\\.",a[i]) == TRUE) i= i+1
                        tryCatch({
                                pathpath = paste(firstFilePath,a[i],"/",sep = "")
                                s = getMatrixData_Maxsat(pathpath)
                                s = s[order(s[,1]), ]
                        },error = function(e){
                                
                        })
                        s = s[, -2]
                        colnames(s) <- c("x", "y")
                        FILE_PATH = save_path %>% paste(save.name, ".csv", sep = "")
                        write.csv(s, file = FILE_PATH, row.names = FALSE )
                }
                
        }
}

transferMaxsatSingleRun <-  function(){
  
  library(dplyr)
  file_path = "../Benchmarking/optimizationBenchmarkingDocu-master/examples/maxSat/results/"
  save_path = "./rawdata/progress-of-future-predict-maxsat/"
  TotalPath = file_path
  respath = save_path
  firstFileList = TotalPath %>% list.files()
  firstFileList = firstFileList[-grep("\\.",firstFileList)]
  count.process = 0
  jFirstLayer = 1
  for(jFirstLayer in 1:length(firstFileList)){
    firstFilePath = paste(TotalPath,firstFileList[jFirstLayer],"/",sep = "")
    filename = paste(firstFileList[jFirstLayer],sep = "")
    a = list.files(firstFilePath)
    i = 1
    for(i in 1:length(a)){
      save.name = paste(filename,"_",a[i],sep = "")
      if(grepl("\\.",a[i]) == TRUE) i= i+1
      tryCatch({
        pathpath = paste(firstFilePath,a[i],"/",sep = "")
        run.file <- pathpath %>% list.files()
        run <- 1
        for(run in 1:length(run.file)){
          s = read.csv(file=paste(pathpath,run.file[run],sep = ''),header = FALSE,sep='')
          s = s %>% as.matrix
          s = s[order(s[, 1]), ]
          s = s[, -2]
          colnames(s) <- c("x", "y")
          FILE_PATH = save_path %>% paste(save.name, "_", run, ".csv", sep = "")
          write.csv(s, file = FILE_PATH, row.names = FALSE )
        }
      },error = function(e){
        
      })
     
    }
    
  }
}



transferTsp <- function(){
  
        library(dplyr)
        file_path = "../Benchmarking/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"
        save_path = "./rawdata/tsp/"
        TotalPath = file_path
        algorithm.listnames = TotalPath %>% list.files()
        for(i in 1:length(algorithm.listnames)){
                instance.filepath <- paste(TotalPath, algorithm.listnames[i], "/symmetric/" ,sep = "")
                instance.listnames <- instance.filepath %>% list.files()
                for(j in 1: (instance.listnames %>% length())){#
                        data.filepath = paste(instance.filepath,instance.listnames[j],sep = "")
                        s = getTheMatrixData_TspSuite(data.filepath)
                        s = s[order(s[,1]),]
                        s = s[, -2]
                        colnames(s) <- c("x", "y")
                        file.name <- paste(algorithm.listnames[i],"_symmetric_", instance.listnames[j], ".csv", sep = "")
                        FILE_PATH <- save_path %>% paste(file.name, sep = "")
                        write.csv(s, file = FILE_PATH, row.names = FALSE )
                }
                
        }
        
}


transferTspSingleRun <- function(){
  
  library(dplyr)
  #raw data file
  file_path = "../Benchmarking/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"
  #raw data save file
  save_path = "./rawdata/tsp-singleRun/"
  if(!file.exists(save_path))
    dir.create(save_path)
  TotalPath = file_path
  #algorithm name list
  algorithm.listnames = TotalPath %>% list.files()
  for(i in 1:length(algorithm.listnames)){
    #instance name list
    instance.filepath <- paste(TotalPath, algorithm.listnames[i], "/symmetric/" ,sep = "")
    instance.listnames <- instance.filepath %>% list.files()
    for(j in 1: (instance.listnames %>% length())){
      #run name list
      data.filepath = paste(instance.filepath,instance.listnames[j], "/", sep = "")
      run.list <- data.filepath %>% list.files()
      #Read Each run data.
      for(run in 1:length(run.list)){
        dirname = paste(data.filepath, run.list[run],sep="")
        con <- file(dirname, "r") #read by line
        line = readLines(con,n=1)
        line=readLines(con,n=1)
        merge.data = as.vector(unlist(strsplit(line,"\\t"))) #list to vector
        while( length(line) != 0 ) {
          line=readLines(con, n=1)
          if(grepl("SECTION_END", line) == TRUE){
            break
          }else{
            line = strsplit(line,"\\t")
            line = as.vector(unlist(line))
            merge.data = rbind(merge.data, line, deparse.level = 0)
          }
        }
        close(con)
        s = merge.data[, 1:6] # Extract data matrix
        s = s[, c(1,6)]
        colnames(s) <- c("x", "y")
        s <- s[order(as.numeric(s[, 1])), ]
        
        file.name <- paste(algorithm.listnames[i], "_symmetric_", instance.listnames[j], "-", run,".csv", sep = "")
        FILE_PATH <- save_path %>% paste(file.name, sep = "")
        write.csv(s, file = FILE_PATH, row.names = FALSE )
        
      }
      
    }
    
  }
  
}


transferBbob <- function(){
  
  library(dplyr)
  file_path = "../BenchMarking/optimizationBenchmarkingDocu-master/examples/bbob/"
  save_path = "./rawdata/bbob/"
  TotalPath = file_path
  #algorithms name
  algorithm.listnames = TotalPath %>% list.files()
  alg.list <- algorithm.listnames[-grep("\\.",algorithm.listnames)]
  
  alg.name.list <- do.call(rbind, strsplit(alg.list, "_"))[, 2]

  for(alg in 1:length(alg.list)){
    #function name
    func.list <- paste(file_path, alg.list[alg], sep = "") %>% list.files()
    func.list <- func.list[-grep("\\.",func.list)]
    
    func = 1
    for(func in 1:length(func.list)){
      #DIM list
      dat.list = paste(file_path, alg.list[alg], "/",func.list[func], sep = "") %>% list.files(pattern = ".*?\\.dat")
      for(dat in 1:length(dat.list)){
        filepath = paste(file_path, alg.list[alg], "/",func.list[func],"/", dat.list[dat], sep = "")
        data = getTheMatrixData_BBOB(filepath)
        save.datname = gsub("bbobexp_","", dat.list[dat]) 
        
        save.file = paste(save_path,
                          alg.name.list[alg],"_",  gsub(".dat",".csv",save.datname), sep = "")
        write.csv(data, file = save.file, row.names = FALSE)
      }
    }
    
  }
}


#ransferTspSingleRun()
transferBbob()
