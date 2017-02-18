library(dplyr)
library(scmamp)
tsp.para.data <- read.csv("./modelresults/LM.tsp.pre/100percentleft/10_all_model.csv")
tsp.para.data %>% head()

tsp.para.data$model

by_model <- group_by(tsp.para.data, tsp.model = model)

tsp.res <- summarise(by_model, n = n()) %>% data.frame()

tsp.res <- tsp.res[order(tsp.res$n, decreasing = TRUE), ]


writeTabular(tsp.res,
             caption = "Best Model in TSP Instances",
             print.row.names = FALSE,  align = "c",
             hrule = 0, vrule = 1,
             wrap.as.table =  TRUE, table.position = "h",
             centering = TRUE,
             caption.position = "t")


maxsat.para.data <- read.csv("./modelresults/LM.maxsat.pre/100percentleft/10_all_model.csv")
maxsat.para.data %>% head()

maxsat.para.data$model

by_model <- group_by(maxsat.para.data, maxsat.model = model)

maxsat.res <- summarise(by_model, n = n()) %>% data.frame()

maxsat.res <- maxsat.res[order(maxsat.res$n, decreasing = TRUE), ]
maxsat.res
