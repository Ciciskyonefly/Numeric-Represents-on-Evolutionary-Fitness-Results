library(quantmod)
?arima

# 
# # example from google
# kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
# kingstimeseries <- ts(kings)
# plot.ts(kingstimeseries)
# #平滑时间序列， 取临近3个窗口的均值
# kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
# plot.ts(kingstimeseriesSMA3)
# kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
# plot.ts(kingstimeseriesSMA8)
# 
# births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
# births
# birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
# birthstimeseries
# plot.ts(birthstimeseries)
# birthstimeseriescomponents <- decompose(birthstimeseries)
# plot(birthstimeseriescomponents)
# birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
# plot(birthstimeseriesseasonallyadjusted)

source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")

library(dplyr)

dat.name <- "ts0r0f_symmetric_berlin52-5.csv"
dat.path <- paste("./rawdata/tsp-singleRun/", dat.name, sep = "")
data <- read.csv(dat.path)
file.path <- "./modelresults/LM.tsp.pre/singleRun/maxTrain-maxTest/"
list.file <- file.path %>% list.files()
para.data <- read.csv(paste(file.path, list.file[7], sep = ""))

x <- gsub("_","/", dat.name) 
 xx <- gsub(".csv", "", x)
par <- para.data[grep(xx, para.data$instance_file), c("a", "b", "c", "d")] %>% as.numeric() %>% as.vector 
par %>% class

traindata <- gompertzModelpositive$modelFunction(par, c(1:50))
traindataSeries <- ts(traindata)
plot.ts(traindataSeries, log = "x")
#HoltWinters

traindataSeriesarima <- arima(traindataSeries, order=c(3, 2, 3))
traindataSeriesarima
library("forecast") # load the "forecast" R library
traindataSeriesarimaforcastes <- forecast.Arima(traindataSeriesarima, h= 50)
traindataSeriesarimaforcastes
plot.forecast(traindataSeriesarimaforcastes)
points(data$x, data$y)
# plot(c(1:50), traindata)
# which(data$x < 50)


# ar(traindataSeries)
# acf(traindataSeries)$acf
# pacf(traindataSeries)$acf
