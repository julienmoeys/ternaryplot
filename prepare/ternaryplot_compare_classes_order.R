
library( "ternaryplot" )



# HYPRES ===================================================

hypres <- getTernarySystem( "hypres" ) 

#   Order as it is now
as.data.frame( hypres, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( hypres, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( hypres[[ "classes" ]][ o, ] )

