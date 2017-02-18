plotCurve_exp_3_single <- function(s,theta){
  
  
  #1,1
  
  s = s[order(s[,1]),]
  #1,2 右上
  x = c(t(s[,1]))
  y = c(t(s[,3]))
  
  if(length(theta) == 3){
          y1 = theta[1]/(1+exp(theta[2]*log(x)+theta[3]))
  }else
          y1 = theta[1]+theta[2]/(1+exp(theta[3]*log(x)+theta[4]))
  
  plot(x,y,col = "BLACK",log = "x",xlab = "FEs",ylab = "F",pch = 3)
  lines(x,y1,col = "BLUE",lwd = 2)
  
}