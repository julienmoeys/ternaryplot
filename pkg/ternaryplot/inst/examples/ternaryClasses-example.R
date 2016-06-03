
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "dummy" )


#   Extract the ternary classes
tc   <- ternaryClasses( s )


#   Plot the ternary system and the grid
#   * Set the plot style (optional)
ternaryStyle( margin = TRUE ) 
#   * Base plot (already with classes)
ternaryPlot( s = s ) 
#   * Overlay of ternary classes
ternaryPolygons( s = tc, border = "darkred", lty = 3 ) 


# #   Alternatively:
tcSp <- ternary2SpatialPolygons( tc ) 

# #  Add sp overlay
# ternaryPlot( s = s ) 
# sp::plot( tcSp, add = TRUE, border = "darkred", lty = 3 ) 

tpPar( reset = TRUE )
