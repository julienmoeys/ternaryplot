
library( "soiltexture2" )



# HYPRES ===================================================

hypres <- getTernarySystem( "hypres" ) 

#   Order as it is now
as.data.frame( hypres, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( hypres, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( hypres[[ "classes" ]][ o, ] )



# USDA =====================================================

usda <- getTernarySystem( "usda" ) 

#   Order as it is now
as.data.frame( usda, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( usda, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( usda[[ "classes" ]][ o, ] )



# Aisne ====================================================

aisne <- getTernarySystem( "aisne" ) 

#   Order as it is now
as.data.frame( aisne, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( aisne, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( aisne[[ "classes" ]][ o, ] )



# GEPPA ====================================================

geppa <- getTernarySystem( "geppa" ) 

#   Order as it is now
as.data.frame( geppa, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( geppa, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( geppa[[ "classes" ]][ o, ] )



# BK94 =====================================================

bk94 <- getTernarySystem( "bk94" ) 

#   Order as it is now
as.data.frame( bk94, what = "centroids" )[, "abbrev" ]

#   Order as it should be
(o <- sort( bk94, decreasing = FALSE, by = c( "CLAY", "SAND" ) )[, "abbrev" ])

#   'classes' slot, reordered
dput( bk94[[ "classes" ]][ o, ] )
