plotCurve_line_logy_logx3_single <- function(s,theta){
        
        #布局    
        s = s[order(s[,1]),]
        x = c(t(s[,1]))
        y = c(t(s[,3]))
        
        if(length(theta) == 3){
                y2 = theta[1]*exp(theta[2]*log(theta[3]+x))
        }else
                y2 = theta[1]+theta[2]*exp(theta[3]*log(theta[4]+x))
        
        plot(x,y,col = "BLACK",log = "x",xlab = "FEs",ylab = "F")
        print("x")
        print(length(x))
        print("y2")
        print(theta)
        print(length(y2))
        # if(is.nan(y2)) y2[is.nan(y2)] <- 0
        lines(x,y2,col = "BLUE")
}