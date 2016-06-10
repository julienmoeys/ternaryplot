
library( "ternaryplot" )

hypres <- getTernarySystem( s = "hypres" )

#   Reverse the axis direction
blrClock( s = hypres ) 
blrClock( s = hypres ) <- rep( FALSE, 3 ) 

addTernarySystem( s = hypres, name = "hypres_FFF" ) 

hypres_FFF <- getTernarySystem( s = "hypres_FFF" )

identical( hypres, hypres_FFF )
# [1] TRUE

#   Change the axis-variable attribution
blrNames( s = hypres_FFF )
blrNames( s = hypres_FFF ) <- c( "CLAY", "SILT", "SAND" ) 

addTernarySystem( s = hypres_FFF, name = "hypres_FFF", 
    overwrite = TRUE )  

hypres_FFF2 <- getTernarySystem( s = "hypres_FFF" )

identical( hypres_FFF, hypres_FFF2 )

#   Clean-up
deleteTernarySystem( name = "hypres_FFF" )
