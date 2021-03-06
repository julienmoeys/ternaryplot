
library( "ternaryplot" ) 

#   Plot a ternary diagram with some classification
#   * Set the style
ternaryStyle( "margin" = TRUE ) 
#   * Plot diagram
s <- ternaryPlot( "hypres" ) 

#   Extract classification vertices and centroids 
pts1 <- ternary2SpatialPoints( s )
pts2 <- ternary2SpatialPoints( s, what = "centroids" )

#   Add to the existing ternary plot
sp::plot( pts1, add = TRUE, col = "darkred", pch = 1 ) 
sp::plot( pts2, add = TRUE, col = "darkblue", pch = 3 ) 

tpPar( reset = TRUE )
