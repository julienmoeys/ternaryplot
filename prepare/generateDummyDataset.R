
# Generate a dummy dataset of ternary data (texture, etc)
# ==========================================================

#   Working directory
setwd( wd <- sprintf( 
    "C:/Users/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot", 
    Sys.info()[[ "user" ]] ) )

#   Fix the random seed (for reproducibility of random 
#   sampling)
set.seed( 2016012009 ) 

#   Number of samples & number of category
n     <- 200 
nbCat <- 3 
.mean <- 50 # Mean texture
.sd   <- 25 # Mean texture standard deviation

#   Generate athe ternary dataset
terDat <- data.frame(
    "CLAY"  = rnorm( n = n, mean = .mean, sd = .sd ), 
    "SILT"  = rnorm( n = n, mean = .mean, sd = .sd ), 
    "SAND"  = rnorm( n = n, mean = .mean, sd = .sd )  
)   

#   Remove negative values
terDat <- terDat[ 
    !apply( terDat, 1, function(x){any(x<0)} ), ] 

#   Re-adjust n
n <- nrow( terDat )

#   Normalise to 100%
terDat <- (terDat / rowSums( terDat )) * 100 

#   Add a dummy category
terDat[, "GROUP" ] <- sample( x = 1:nbCat, size = n, 
    replace = TRUE )



# Plot the test dataset (no classification)
# ==========================================================

library( "ternaryplot" ) 

#   Set general graphical parameters (enlarged plot)
par( mar = c(3,2,0,2)+.1, family = "serif", font = 2, 
    font.axis = 2, font.lab = 2 ) 
    # mar = c(bottom, left, top, right) 

#   Set specific graphical parameters
tpPar( grid.line.col = "white", arrowsBreak = FALSE, 
    plot.bg = gray( .95 ), axis.line.lwd = 2, 
    class.border.lwd = 2 )

ternaryPlot( s = "default", x = terDat[, 1:3 ], 
    pch = terDat[, "GROUP" ], lwd = 2 )



# Plot the test dataset (HYPRES classification)
# ==========================================================

library( "ternaryplot" ) 

#   Set general graphical parameters (enlarged plot)
par( mar = c(3,2,0,2)+.1, family = "serif", font = 2 ) # mar = c(bottom, left, top, right) 

#   Set specific graphical parameters
tpPar( grid.line.col = "white", arrowsBreak = FALSE, 
    plot.bg = gray( .95 ), axis.line.lwd = 2, 
    class.border.lwd = 2 )

ternaryPlot( s = "hypres", x = terDat, pch = terDat[, "GROUP" ], 
    lwd = 2 )



# Classify (temporary fix)
# ==========================================================

#   Add the vertices (for control)
vert <- getTernarySystem( s = "hypres" )[[ "vertices" ]]
vert[, "GROUP" ] <- 4L 
terDat <- rbind( terDat, vert[, colnames( terDat ) ] )

#   Convert ternary points to sp SpatialPoints
xyDat <- ternary2xy( s = "hypres", x = terDat )
spPts <- sp::SpatialPoints(coords = xyDat[ ,c( "x", "y" ) ] ) 

#   Convert ternary classes to sp SpatialPolygonsDataFrame 
#s    <- getTernarySystem( s = "hypres" ) 
cls   <- ternaryClasses( s = "hypres" ) # s = s 
spCls <- ternary2SpatialPolygons( x = cls )

#   Classify the ternary data

#   *   Fetch the class ID
clsId <- sapply( spCls@polygons, function(x){slot(x,"ID")} )

#   *   Classify
ptsClass <- 
    clsId[ sp::over( x = spPts, y = as( spCls, "SpatialPolygons" ) ) ] 

#   Number of samples per class
table( ptsClass ) 
 # C  F  M MF VF 
 # 3 70 95  3 11

#   Plot the data with class specific colours
#   (for verification of the classification)
ternaryPlot( s = "hypres", x = terDat, pch = terDat[, "GROUP" ], 
    lwd = 2, col = as.integer( as.factor( ptsClass ) ) )
