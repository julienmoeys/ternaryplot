
library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
#   (which has not classes)
s <- ternaryPlot( s = "default" ) 


#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tg   <- createTernaryGrid( s )


#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPolygons( s = tg, border = "darkred", lty = 2 ) 


# #   Alternatively:
# ternaryPlot( s = s )
# tgSp <- ternary2SpatialPolygonsDataFrame( tg ) 
# sp::plot( tgSp, add = TRUE, border = "darkred", lty = 3 ) 
