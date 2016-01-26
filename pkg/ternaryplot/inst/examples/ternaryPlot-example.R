
library( "ternaryplot" ) 

# Simple example, with point overlay
# ==================================

#   Save graphical parameters (to reset them later)
op <- par( no.readonly = TRUE ) 

#   Define the plot style (optional)
ternaryStyle( margin = TRUE ) 

#   Load an example dataset ( dummy example )
data( "textureDataset", package = "ternaryplot" ) 

#   Simple ternary plot with point overlay
ternaryPlot( 
    s   = "default", 
    x   = textureDataset[, c( "SAND", "CLAY", "SILT" ) ], 
    pch = textureDataset[, "GROUP" ] ) 



# Same example, with a ternary classification
# ===========================================

#   Simple ternary plot with point overlay
ternaryPlot( 
    s   = "hypres", 
    x   = textureDataset, 
    pch = textureDataset[, "GROUP" ] ) 

#   Notice that in the example above "hypres" classification 
#   *requires* columns named CLAY, SILT and SAND and 
#   will not take any other column names.



# Same example, with a ternary classification
# and custom column names for the texture classes
# ===========================================

#   Copy the dataset
textureDataset2 <- textureDataset[, c( "CLAY", "SILT", 
    "SAND", "GROUP" ) ]

#   Rename columns
colnames( textureDataset2 ) <- c( "clay", "silt", "sand", 
    "group" ) 

#   Fetch the definition of the ternary classification
hypres <- getTernarySystem( "hypres" ) 

#   Set the column names
#   Note: the order matter!
blrNames( hypres ) <- c( "sand", "clay", "silt" ) 

#   Set new labels
blrLabels( hypres ) <- c( "sand [%]", "clay [%]", "silt [%]" ) 

#   Simple ternary plot with point overlay
ternaryPlot( 
    s   = hypres, 
    x   = textureDataset2, 
    pch = textureDataset2[, "group" ] )



# Vary the plot geometry
# ======================

# Prepare the plot layout
# -----------------------

#   Reset plot parameters
par( op )
tpPar( reset = TRUE )

#   Split the window in 4 sub-plots
par( mfrow = c(2,2) )

#   Set the plot style (optional)
ternaryStyle() 



# Default ternary plot
# --------------------

s <- ternaryPlot() 


# Change axis orientation (clockwise vs counter-clockwise)
# --------------------------------------------------------

#   Set new orientation
blrClock( s ) <- rep( FALSE, 3 ) 

#   Plot
ternaryPlot(s) 


# Change axis orientation and frame angles
# ----------------------------------------

#   Set new orientation and angles
blrClock( s )  <- c( FALSE, TRUE, NA ) 
tlrAngles( s ) <- c( 45, 90, 45 ) 

#   Plot
ternaryPlot(s) 


# Same, other direction
# ---------------------

blrClock( s ) <- c( TRUE, NA, FALSE ) 
tlrAngles( s ) <- c(45,45,90) 

ternaryPlot(s) 



#   reset par (completely)
par( op )
tpPar( reset = TRUE )
