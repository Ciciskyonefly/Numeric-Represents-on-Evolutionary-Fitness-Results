
library(dplyr)
library(stringr)

f.bbob = do.call(rbind, strsplit(list.files("./rawdata/bbob/"), "_"))[, 2] %>% unique()
dim.bbob = do.call(rbind, strsplit(list.files("./rawdata/bbob/"), "_"))[, 3] %>% unique()

algorithms.bbob = do.call(rbind, strsplit(list.files("./rawdata/bbob/"), "_"))[, 1] %>% unique()
f.bbob = f.bbob[str_extract(f.bbob, "[0-9]+") %>% as.numeric %>% order()]
dim.bbob = dim.bbob[str_extract(dim.bbob, "[0-9]+") %>% as.numeric %>% order()]
dim.bbob = gsub(".csv", "", dim.bbob)
