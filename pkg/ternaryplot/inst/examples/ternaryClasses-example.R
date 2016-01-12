
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "dummy" )

#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tc   <- ternaryClasses( s )

#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPlot( s = tc, add = FALSE, 
    polygonExtra = list( border = "red", lty = 2 ) ) 

# #   Alternatively:
tcSp <- ternary2SpatialPolygonsDataFrame( tc ) 
# sp::plot( tcSp, add = TRUE, border = "darkred", lty = 3 ) 
