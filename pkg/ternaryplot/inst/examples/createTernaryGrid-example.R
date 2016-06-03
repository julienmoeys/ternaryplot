
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
#   (which has not classes)
s <- ternaryPlot( s = "default" ) 


#   Create the ternary grid 
tg   <- createTernaryGrid( s )


#   Plot the ternary system and the grid
#   * Set the plot style (optional)
ternaryStyle( margin = TRUE ) 
#   * Base plot
ternaryPlot( s = s )
#   * Grid overlay
ternaryPolygons( s = tg, border = "darkred", lty = 3 ) 


# #   Alternatively:
tgSp <- ternary2SpatialPolygons( tg ) 

# #   sp overlay
# ternaryPlot( s = s )
# sp::plot( tgSp, add = TRUE, border = "darkred", lty = 3 ) 

tpPar( reset = TRUE )
