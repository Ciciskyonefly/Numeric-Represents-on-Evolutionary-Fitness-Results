Myggolot2.label <- function(){
        
        labs(
                x = "FEs",
                y = "Fitness Value",
                colour = "Cylinders"
        ) 
        
}

Myggolot2.theme <- function(){
        
        
        theme( 
             #   plot.title = element_text(size=22),
             #   size = 0.5,
                aspect.ratio = 1,
                panel.grid.major = element_line( linetype = 3 , colour = "lightgrey" ),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black" )
          #      axis.text.x = element_text(size = 15, hjust = 1),
          #      axis.text.y = element_text(size = 15, hjust = 1)
        )
}