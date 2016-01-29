
library( "ternaryplot" ) 
library( "rgeos" ) 

s   <- getTernarySystem( "hypres" ) 
pol <- ternary2SpatialPolygons( s )

sp::proj4string( obj = pol ) <- "+proj=utm" 

pol <- as( pol, "SpatialLines" )

data( "textureDataset", package = "ternaryplot" ) 

textureDataset2 <- rbind(
    textureDataset, 
    data.frame( 
        s[[ "vertices" ]][, c( "CLAY", "SILT", "SAND" ) ], 
        "GROUP" = 4L ) 
)   

pts <- ternary2SpatialPoints( x = textureDataset2, s = s ) 

sp::proj4string( obj = pts ) <- "+proj=utm" 

plot( pol ) 
plot( pts, add = TRUE )

dst <- rgeos::gDistance( pts, pol, byid=TRUE )
dst <- apply( X = dst, MARGIN = 2, FUN = min ) 

pch <- rep( 1L, length( dst ) ) 
pch[ dst < 0.2 ] <- 2L

ternaryPlot( s = "hypres", x = textureDataset2, pch = pch )
