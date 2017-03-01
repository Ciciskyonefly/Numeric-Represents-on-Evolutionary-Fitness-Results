C.dat.median <- "./analysis-compare/res-h_percents_error_in_prediction/Tsp_C_maxTrain_maxTest_single_run.csv"
P.dat.median <- "./analysis-compare/res-h_percents_error_in_prediction/Tsp_P_maxTrain_maxTest_single_run.csv"
Wei.dat.median <- "./analysis-compare/res-h_percents_error_in_prediction/Tsp_weighted_maxTrain_maxTest_single_run.csv"
C.dat.median <- read.csv(C.dat.median)
P.dat.median <- read.csv(P.dat.median)
Wei.dat.median <- read.csv(Wei.dat.median)
colnames(C.dat.median)
colnames(P.dat.median)
colnames(Wei.dat.median)
round(C.dat.median[, c(2:10)]/P.dat.median[, c(2:10)], 3)
round(Wei.dat.median[, c(2:10)]/P.dat.median[, c(2:10)], 3)
P.dat.median[, c("X10_10", "X100_10")] <- NULL
P.dat.median[, c("X50_10")] <- NULL
C.dat.median[, c("X10_10", "X100_100")] <- NULL
Wei.dat.median[, c("X100_100")] <- NULL
Wei.dat.median <- Wei.dat.median[, c(1:4, 8:9, 5:7)]
round(C.dat.median[, c(2:9)]/P.dat.median[, c(2:9)], 3)
res.C.P <- cbind(C.dat.median[, 1], round(C.dat.median[, c(2:9)]/P.dat.median[, c(2:9)], 3))
res.W.P <- cbind(C.dat.median[, 1], round(Wei.dat.median[, c(2:9)]/P.dat.median[, c(2:9)], 3))

write.csv(res.C.P, "results_of_C_divide_P.csv", row.names = FALSE)
write.csv(res.W.P, "results_of_W_divide_P.csv", row.names = FALSE):w

