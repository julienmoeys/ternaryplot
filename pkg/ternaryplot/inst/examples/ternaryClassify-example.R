
library( "ternaryplot" ) 

# Classify ternary points with a ternary classification
# =====================================================

#   Example with the HYPRES soil texture triangle

#   Load an example dataset ( dummy example )
data( "textureDataset", package = "ternaryplot" ) 

#   Classify the dataset
cls <- ternaryClassify( s = "hypres", x = textureDataset ) 

#   Tabular statistics
table( cls )

#   Classify the dataset (full details)
cls2 <- ternaryClassify( s = "hypres", x = textureDataset, 
    method = "point.in.polygon" ) 

#   View the output (1st rows)
head( cls2 )

#   Any point on an edge?
any( !(cls2 %in% 0:1 ) )



# Plot the result
# ---------------

#   Convert class (text) into integers, 'col'-codes
cols <- as.integer( factor( cls ) )

#   Save graphical parameters (to reset them later)
op <- par( no.readonly = TRUE ) 

#   Define the plot style (optional)
ternaryStyle( margin = TRUE ) 

#   Simple ternary plot with point overlay
ternaryPlot( s = "hypres", x = textureDataset, col = cols ) 



# Classify ternary points in a grid
# =================================

grd <- createTernaryGrid( s = "hypres" ) 

#   Classify the dataset
cls3 <- ternaryClassify( s = grd, x = textureDataset ) 

#   Counts per grid cells
counts <- ternaryCount( s = grd, x = textureDataset )
counts <- attr( x = counts, which = "data" ) 

cols <- gray( 1 - counts[, "counts" ] / max( counts[, "counts" ] ) )

#   Simple ternary plot with point overlay
s <- ternaryPlot( s = "hypres" ) 
ternaryPolygons( s = grd, bg = cols )
ternaryPoints( s = s, x = textureDataset )



#   Reset plot parameters
par( op )
tpPar( reset = TRUE )


