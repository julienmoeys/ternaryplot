
library( "ternaryplot" ) 


# Split the window in 4 sub-plots
# -------------------------------

#   Save and then set graphical parameters
op <- par() 
par( mfrow = c(2,2) )


# Define the plot style (optional)
# --------------------------------

ternaryStyle( margin = TRUE ) 


# Default ternary plot
# ====================

s <- ternaryPlot() 


# Change axis orientation (clockwise vs counter-clockwise)
# ========================================================

#   Set new orientation
blrClock( s ) <- rep( FALSE, 3 ) 

#   Plot
ternaryPlot(s) 


# Change axis orientation and frame angles
# ========================================

#   Set new orientation and angles
blrClock( s )  <- c( FALSE, TRUE, NA ) 
tlrAngles( s ) <- c( 45, 90, 45 ) 

#   Plot
ternaryPlot(s) 


# Same, other direction
# =====================

blrClock( s ) <- c( TRUE, NA, FALSE ) 
tlrAngles( s ) <- c(45,45,90) 

ternaryPlot(s) 


#   reset par
par( "mfrow" = op[[ "mfrow" ]] )



# Ternary plot with ternary classification
# ========================================

ternaryPlot( "hypres" ) 



#   reset par (completely)
par( op )
tpPar( reset = TRUE )
