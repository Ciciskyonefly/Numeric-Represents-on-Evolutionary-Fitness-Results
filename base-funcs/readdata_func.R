
getTheMatrixData_TspSuite <- function(filePath){
  # Read TSP problem every instance data
  # Args:
  #   filePath: The path of one instance.
  #
  # Return:
  #   A matrix of one TSP instance' fitness data.
  
  
  a = list.files(filePath)
  
  dirname = paste(filePath,"/",a[1],sep="")
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
  #############
  
  
  
  #############
  for(i in 2:length(a)){
    dirname = paste(filePath,"/",a[i],sep="")
    
    con <- file(dirname, "r")
    line = readLines(con,n=1)
    line=readLines(con,n=1)
    while( length(line) != 0 ) {
      line=readLines(con,n=1)
      if(grepl("SECTION_END",line) == TRUE){
        break
      }else{
        line = strsplit(line,"\\t")
        line = as.vector(unlist(line))
        merge.data = rbind(merge.data,line,deparse.level = 0)
      }
    }
    close(con)
  }
  
  
  s = merge.data[, 1:6] # Extract data matrix
  s[, 3] = s[, 6]
  s = s[, 1:3]
  s=matrix(as.numeric(s), nrow=nrow(s)) # Turn to numeric matrix
  return (s)
} 




getMatrixData_Maxsat<-function(filePath){
  # Read maxsat problem data
  # Args:
  #   filePath: The path of one instance.
  #
  # Return:
  #   A matrix of one maxsat instance' fitness results data.
  
  
  a = list.files(filePath) 
  m = length(a)
  merge.data = read.table(file=paste(filePath,a[1],sep = ''),header = FALSE,sep='')
  for(j in 2:m){
    new.data = read.table(file = paste(filePath,a[j],sep = ''),header= FALSE,sep='')
    merge.data = rbind(merge.data,new.data)
  }
  merge.data = as.matrix(merge.data)
  merge.data = merge.data[order(merge.data[, 1]), ]
  return (merge.data)
}




getTheMatrixData_BBOB <- function(filePath){
  con <- file(filePath, open = "r")
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
  
  
  s = merge.data[,c(1,3)]
  s = s[-1, ]
  s = matrix(as.numeric(s),nrow=nrow(s)) %>% data.frame()
  s = s[order(s[,1]), ]
  colnames(s) = c("x", "y")
  return (s)
}  

