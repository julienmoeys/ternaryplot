
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
plot( pts1, add = T, col = "darkred", pch = 1 ) 
plot( pts2, add = T, col = "darkblue", pch = 3 ) 

