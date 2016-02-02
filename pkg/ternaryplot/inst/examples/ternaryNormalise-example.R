
library( "ternaryplot" )


#   Load an example dataset ( dummy example )
data( "textureDataset", package = "ternaryplot" ) 


#   Copy, subset and alter the dataset, so that the values 
#   are not summing to 100% any longer
textureDataset2 <- textureDataset[ 1:4, ] 

textureDataset2[, "CLAY" ] <- 
    textureDataset2[, "CLAY" ] + c( -1, 0, 0, 0 ) 

textureDataset2[, "SILT" ] <- 
    textureDataset2[, "SILT" ] + c( 0, 1, 0, 0 ) 

textureDataset2[, "SAND" ] <- 
    textureDataset2[, "SAND" ] + c( 0, 0, 1, 0 ) 


#   Normalise the dataset to 100% again
textureDataset2n <- ternaryNormalise( 
    s = "hypres", 
    x = textureDataset2 )


#   Check the sum of 3 fractions (also done internally in 
#   ternaryNormalise)
rowSums( textureDataset2n[, c( "CLAY", "SILT", "SAND" ) ] ) 

#   Fetch the residuals (of the normalisation)
textureDataset2n[, "residuals" ]


# # Would also work
# s <- ternaryPlot( "hypres" ) 
# textureDataset2n <- ternaryNormalise( 
    # s = s, 
    # x = textureDataset2 )
# ternaryPoints( s = s, x = textureDataset2n ) 

