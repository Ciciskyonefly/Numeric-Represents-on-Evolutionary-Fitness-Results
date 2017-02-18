
library(scmamp)
all.residuals <- read.csv("./analysis-compare/maxsat_parameters_prediction/maxsat_residuals_predict_parameters.csv")
FlipHCrs.readiuals <- all.residuals[all.residuals$algorithms == "2FlipHCrs", c( 2, 7, 8)]%>% data.frame()


colnames(FlipHCrs.readiuals) <- c( "Instances scale", "Predicted Parameters", "Modeling Parameters")


FlipHCrs.readiuals$`Instances scale` <- paste("uf", FlipHCrs.readiuals$`Instances scale` ,sep = "")

save.maxsat.path = "../paper-latex/maxsatpredictionparametersresidual.tex"
writeTabular(FlipHCrs.readiuals,
             file = save.maxsat.path,
             caption = "2FlipHCrs Residuals Comparison on Predict Parameters and Modeling Parameters",
             align = "c",
             wrap.as.table = TRUE,
             table.position = "hbtp",
             caption.position = "t",
             print.row.names = FALSE,
             centering = TRUE
)



