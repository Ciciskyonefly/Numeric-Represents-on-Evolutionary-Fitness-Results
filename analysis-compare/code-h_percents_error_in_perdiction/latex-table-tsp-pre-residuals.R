library(scmamp)
all.residuals <- read.csv("./analysis-compare/tsp_parameters_prediction/residuals_tsp_predict_parameters.csv")
ts10r10b.residuals <- all.residuals[all.residuals$algorithms == "ts10r10b", c(2,3,4)]%>% data.frame()

 ts1e7r0f.residuals <- all.residuals[all.residuals$algorithms == "ts1e7r0f", c(2,3,4)]%>% data.frame()


colnames(ts10r10b.residuals) <- c( "Instances scale", "Predicted Parameters", "Modeling Parameters")
colnames(ts1e7r0f.residuals) <- c( "Instances scale", "Predicted Parameters", "Modeling Parameters")


writeTabular(ts10r10b.residuals,
             #   file = allresiduals.path,
             caption = "ts10r10b Residuals Comparison on Predict Parameters and Modeling Parameters",
             align = "c",
             wrap.as.table = TRUE,
             table.position = "hbtp",
             caption.position = "t",
             print.row.names = FALSE,
             centering = TRUE
)

writeTabular( ts1e7r0f.residuals,
             #   file = allresiduals.path,
             caption = "ts1e7r0f Residuals Comparison on Predict Parameters and Modeling Parameters",
             align = "c",
             wrap.as.table = TRUE,
             table.position = "hbtp",
             caption.position = "t",
             print.row.names = FALSE,
             centering = TRUE
)
