plotCurve_tweise_single <- function(s,theta){
        
        s = s[order(s[,1]),]
        x = c(t(s[,1]))
        y = c(t(s[,3]))
        if(length(theta) == 3){
                y1 = (theta[1]*exp(theta[2]*x^theta[3]))
        }else
                y1 = theta[1]+(theta[2]*exp(theta[3]*x^theta[4]))
        
        plot(x,y,log = "x",xlab = "FEs",ylab = "F", pch = 3)
        
        length(y1)
        lines(x,y1,col = "BLUE",lwd = 2)
}