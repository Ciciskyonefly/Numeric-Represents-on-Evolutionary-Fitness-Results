rm(list = ls())

weights_funcs <- function(y){
    # Return LM algorithms weights value  
    #
    # Args:
    #   y: The vector gonna to be weights, but have some restrains.   
    #
    # Returns:
    #   LM algorithms weights.
    
    if(which(y < 0) %>% length != 0){
        weights <- (1/abs(y- min(y) + 0.1))
        
    } else {
        if(which(y == 0) %>% length() != 0){
            y[which(y == 0)] = min(abs(y[-which(y == 0)]))/2
        }
        weights <- 1/abs(y)
    }
    
    return (weights)
}


# y <- c(-1, -1, 2)
# weights_funcs(y)
