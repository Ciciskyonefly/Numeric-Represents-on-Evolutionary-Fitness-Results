library(scmamp)
head(data.gh.2008)

nm <- nemenyiTest(data.gh.2008)

nm$diff.matrix



plotCD(results.matrix = data.gh.2008, alpha = 0.05)

test.res <- postHocTest(data = data.gh.2008, test = 'friedman', correct = 'bergmann')
bold <- test.res$corrected.pval  < 0.05
bold[is.na(bold)] <- FALSE

writeTabular(table = test.res$corrected.pval,file = "hello.tex", format = 'f', bold = bold, hrule = 0, vrule = 0)


writeTabular(table = iris ,file = "hello.tex", format = 'f',  hrule = 0, vrule = 0)
