
library( "ternaryplot" ) 

#   Load an example dataset ( dummy example )
data( "textureDataset", package = "ternaryplot" ) 



# Count the number of data-points per grid-cell
# =====================================================

counts1 <- ternaryCount( s = "hypres", x = textureDataset ) 

attr( x = counts1, which = "counts" )



# Count the number of data-points per ternary class
# =====================================================

counts2 <- ternaryCount( s = "hypres", x = textureDataset, 
    grid = FALSE ) 

attr( x = counts2, which = "counts" )


