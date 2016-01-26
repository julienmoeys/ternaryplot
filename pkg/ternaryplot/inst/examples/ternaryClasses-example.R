
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "dummy" )

#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tc   <- ternaryClasses( s )

#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPolygons( s = tc, border = "darkred", lty = 3 ) 

# #   Alternatively:
tcSp <- ternary2SpatialPolygons( tc ) 
# ternaryPlot( s = s ) 
# sp::plot( tcSp, add = TRUE, border = "darkred", lty = 3 ) 
