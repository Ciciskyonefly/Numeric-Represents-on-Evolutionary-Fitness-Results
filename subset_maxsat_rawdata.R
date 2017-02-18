xx <- list.files("./rawdata/progress-of-future-predict-maxsat/")
file.path = "./rawdata/progress-of-future-predict-maxsat/"
library(stringr)
#rm experiment.csv
# rm.list1 <- paste(file.path, xx[grep("experiment", xx)], sep = "")
# file.remove(rm.list1)
yy <-  do.call(rbind, strsplit(xx, "_|-")) %>% data.frame()

runs.value <- gsub( ".csv", "", yy$X4 %>% as.character) %>% as.numeric
index <- which(runs.value %% 2 == 0) 
rm.list2 <- paste(file.path, xx[index], sep = "")

file.remove(rm.list2)

yy$X3 %>% as.character() %>% as.numeric()
  