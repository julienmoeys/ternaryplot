
library( "ternaryplot" )

hypres <- getTernarySystem( s = "hypres" )

#   Reverse the axis direction
blrClock( s = hypres ) 
# [1] TRUE TRUE TRUE
blrClock( s = hypres ) <- rep( FALSE, 3 ) 

addTernarySystem( s = hypres, name = "hypres_FFF" ) 

"hypres_FFF" %in% listTernarySystem()[, "systemName" ]
# [1] TRUE

deleteTernarySystem( name = "hypres_FFF" ) 

"hypres_FFF" %in% listTernarySystem()[, "systemName" ]
# [1] FALSE
