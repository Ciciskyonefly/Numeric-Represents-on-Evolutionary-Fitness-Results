file <- "./modelresults/LM.tsp.pre/singleRun/all_train_test_weighted_merge_yData_error.csv"

data.matrix <- LoadTsp(file)
#  data.matrix <- data.matrix[-grep("experiment",data.matrix$instances %>% as.character ), ]

temp.pre.data <- data.matrix
if(which(is.na(temp.pre.data$residuals)) %>% length() != 0){
    temp.pre.data <- temp.pre.data[-which(is.na(temp.pre.data$residuals)), ]
}

split.resi <- split(temp.pre.data, list(temp.pre.data$instances, temp.pre.data$algorithms), drop = TRUE)


head(split.resi)

stat.Mean <- NULL
stat.Median <- NULL
stat.SE_Mean <- NULL
stat.Variance <- NULL
stat.Stdev <- NULL

split <- 1
for(split in 1:length(split.resi)){
    base.stat <- split.resi[[split]][, c(2:10)] %>% basicStats()
    
    stat.Median <- rbind(stat.Median, base.stat["Median", ])
    stat.Mean <- rbind(stat.Mean, base.stat["Mean", ])
    stat.SE_Mean <- rbind(stat.SE_Mean, base.stat["SE Mean", ])
    stat.Variance <- rbind(stat.Variance, base.stat["Variance", ])
    stat.Stdev <- rbind(stat.Stdev, base.stat["Stdev", ])
    
}

rownames(stat.Mean) <- names(split.resi)
rownames(stat.Median) <- names(split.resi)
rownames(stat.SE_Mean) <- names(split.resi)
rownames(stat.Variance) <- names(split.resi)
rownames(stat.Stdev) <- names(split.resi)


savecsv.names = paste("./analysis-compare/res-h_percents_error_in_prediction/Tsp_weighted_maxTrain_maxTest_single_run.csv" , sep = "")
write.csv(round(stat.Median, 5),file =  savecsv.names, row.names = TRUE)

