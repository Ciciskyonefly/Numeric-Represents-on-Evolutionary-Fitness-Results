library(ggplot2)

p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))



update_geom_defaults("point", aes(colours = "darkblue"))
ggplot(mpg, wt, data = mtcars)

d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
d + stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 2)

d+update_geom_defaults("point", aes(colours = "darkblue"))

update_geom_defaults("point", aes(colour = "darkblue"))
qplot(mpg, wt, data = mtcars)
update_geom_defaults("point", aes(colour = "black"))
qplot(mpg, wt, data = mtcars)


qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy/pop , uempmed, data = economics, geom = c("point", "path"))


qplot(unemploy/pop , uempmed, data = economics, geom = c("point", "path") , colour = year(date)) + scale_color_continuous()
+ scale_fill_continuous(low="darkgreen", high="orangered", space='rgb')



#change legend name : gudies guide_legend
set.seed(100)
dms <- diamonds[sample(nrow(diamonds),500),]
p  <- ggplot(data=dms, aes(x=carat, y=price, color=cut)) + guides(color = guide_legend( title = "Hello"))

p + geom_point()
