
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "default" ) 

#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tg   <- createTernaryGrid( s )
tgSp <- ternary2SpatialPolygonsDataFrame( tg )

#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPlot( s = tg, add = FALSE, 
    polygonExtra = list( border = "red", lty = 2 ) ) 

# #   Alternatively:
# sp::plot( tgSp, add = TRUE, border = "darkred", lty = 3 ) 
