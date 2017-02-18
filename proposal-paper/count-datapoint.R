rm(list = ls())
library(dplyr)
problem.datafile = "./rawdata/maxsat/"
file.name <- list.files(problem.datafile)
file.name <- file.name[-grep("_experiment", file.name)]
count <- 0
for(i in 1:length(file.name)){
        data <- read.csv(paste(problem.datafile, file.name[i], sep = ""))
        count <- count + nrow(data)
}

rm(list = ls())
library(dplyr)
problem.datafile = "./rawdata/tsp/"
file.name <- list.files(problem.datafile)
#file.name <- file.name[-grep("_experiment", file.name)]
count <- 0
for(i in 1:length(file.name)){
        data <- read.csv(paste(problem.datafile, file.name[i], sep = ""))
        count <- count + nrow(data)
}

print(count)