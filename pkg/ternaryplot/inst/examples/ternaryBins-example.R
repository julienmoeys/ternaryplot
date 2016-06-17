
library( "ternaryplot" )

data( "textureDataset", package = "ternaryplot" ) 

# 'sandwitch' plotting of different plot components (with bins)
# ==========================================================

ternaryPlot( "hypres" )
s <- ternaryPlot( "hypres", axes = FALSE, grid = FALSE, 
    classes = FALSE ) 
cnts <- ternaryBins( s = s, x = textureDataset ) 
ternaryPolygons( s = s, bg = NA ) 
ternaryGrid( s = s )
ternaryBox( s = s )
ternaryAxis( s = s )
ternaryPoints( s = s, x = textureDataset ) 

head( attr( cnts, "data" ) ) 

# The same, all in one
# ==========================================================

ternaryPlot( s = "hypres", x = textureDataset, 
    type = c( "p", "c" ) )
