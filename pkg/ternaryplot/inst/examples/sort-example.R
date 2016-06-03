
library( "ternaryplot" )

hypres <- getTernarySystem( "hypres" )

sort( hypres, by = c( "CLAY", "SAND" ), decreasing = TRUE ) 

sort( hypres, by = c( "CLAY", "SAND" ), decreasing = FALSE ) 

#   Compared to 
as.data.frame( hypres, what = "centroids" )
