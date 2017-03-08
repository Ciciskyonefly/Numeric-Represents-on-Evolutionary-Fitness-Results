
file <- "./rawdata/bbob-1-log-y/CMAES_f12_DIM10.csv"
dat <- read.csv(file)
if(which$dat)
    
    plot(dat$x, dat$y, log = "x")

tail(dat)
?rep
?seq(1003, 10000)
dat <- rbind(dat, added_dat %>% data.frame)

added_dat <- cbind(seq(1003, 10000), rep(0.98, 8998))
dat
added_dat %>% data.frame %>% class()
dat %>% class()
