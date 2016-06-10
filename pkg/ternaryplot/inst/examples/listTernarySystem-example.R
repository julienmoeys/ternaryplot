
library( "ternaryplot" ) 

listTernarySystem()

#   Notice the difference
lts <- listTernarySystem( definition = TRUE )
lts 

names( lts[ 1L, "ternarySystem" ] )
class( lts[ 1L, "ternarySystem" ][[ 1L ]] )
