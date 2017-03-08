source("./base-funcs/maxsattsp_load_and_order_func.R")
source("./base-funcs/models_var.R")
source("./base-funcs/models_func.R")
library(cluster)
file <- "./modelresults/LM.bbob.pre/bbob-1-log-y/10_GPMP.csv"
qipa <- LoadBbob(file)
#head(qipa)

split.dat <- split(qipa, list(qipa$alg, qipa$func) )
split.dat %>% length()

pdf.name <- "bbob_change.pdf"
pdf(pdf.name)
par(mfrow = c(2, 2))
i = 1
for(i in 1:length(split.dat)){
    tmp.dat <- split.dat[[i]]
    fi <- 1
    for(fi in 1:nrow(tmp.dat)){
        
        rawdata.name <- gsub("/", "_", tmp.dat[fi, "instance_file"] %>% as.character)
        par <- tmp.dat[fi, c("a", "b", "c", "d")] %>% as.numeric
        formula <-  eval(parse(text = tmp.dat$model[fi] %>% as.character))$formula
        x <- seq(1, 1000000, by = 10)
        y <- GetFormulaValue(formula, x, par)[, 2]
        if(fi == 1){
            plot(x, y, log = "x", col = fi, type = "l", ylim = c(0, 10))
        } else {
            lines(x, y, col = fi)
        }
    }
 
    title(main = names(split.dat[i]))
    legend("topright", legend = tmp.dat$dim %>% as.character, col = 1:nrow(tmp.dat), lty = 1, cex = 0.8)
}

dev.off()
