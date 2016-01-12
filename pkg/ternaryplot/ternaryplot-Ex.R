pkgname <- "ternaryplot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ternaryplot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("createTernaryGeometry")
### * createTernaryGeometry

flush(stderr()); flush(stdout())

### Name: createTernaryGeometry
### Title: Creates a ternaryGeometry object: ternary plot geometry
###   definition.
### Aliases: createTernaryGeometry

### ** Examples


library( "ternaryplot" ) 


## Default geometry
tg <- createTernaryGeometry()
tg

## Custom geometry
tg <- createTernaryGeometry( 
    "fracSum"   = 1, 
    "blrClock"  = rep( FALSE, 3 ), 
    "tlrAngles" = c( 45, 45, 90 ) ) 
tg 




cleanEx()
nameEx("createTernaryGrid-methods")
### * createTernaryGrid-methods

flush(stderr()); flush(stdout())

### Name: createTernaryGrid
### Title: Create a regular ternary grid. Base frame for binning ternary
###   data.
### Aliases: createTernaryGrid createTernaryGrid.character
###   createTernaryGrid.ternarySystem

### ** Examples


library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "default" ) 

#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tg   <- createTernaryGrid( s )
tgSp <- ternary2SpatialPolygonsDataFrame( tg )

#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPlot( s = tg, add = FALSE, 
    polygonExtra = list( border = "red", lty = 2 ) ) 

# #   Alternatively:
# sp::plot( tgSp, add = TRUE, border = "darkred", lty = 3 ) 



cleanEx()
nameEx("createTernarySystem")
### * createTernarySystem

flush(stderr()); flush(stdout())

### Name: createTernarySystem
### Title: Creates a ternarySystem object: ternary plot system definition.
### Aliases: createTernarySystem

### ** Examples


library( "ternaryplot" ) 


## Default variables
tsy <- createTernarySystem() 
tsy 

## Custom variables
tsy2 <- createTernarySystem( 
    "ternaryVariables" = createTernaryVariables( 
        "blrNames"  = c( "CLAY", "SILT", "SAND" ), 
        "blrLabels" = c( "Clay [%]", "Silt [%]", "Sand [%]" ) 
    ),  
    "ternaryGeometry" = createTernaryGeometry( ##
        "fracSum"   = 100, 
        "tlrAngles" = c( 45, 45, 90 ) # top, left, right angles
    ), 
    "main" = "My ternary plot" ) 

tsy2




cleanEx()
nameEx("createTernaryVariables")
### * createTernaryVariables

flush(stderr()); flush(stdout())

### Name: createTernaryVariables
### Title: Creates a ternaryVariables object: ternary plot variables
###   definitions.
### Aliases: createTernaryVariables

### ** Examples


library( "ternaryplot" ) 


## Default variables
tv <- createTernaryVariables() 
tv 

## Custom variables
tv2 <- createTernaryVariables( 
    blrNames  = c( "CLAY", "SILT", "SAND" ), 
    blrLabels = c( "Clay [%]", "SILT [%]", "SAND [%]" ) ) 
tv2 




cleanEx()
nameEx("ternary2SpatialPolygonsDataFrame-methods")
### * ternary2SpatialPolygonsDataFrame-methods

flush(stderr()); flush(stdout())

### Name: ternary2SpatialPolygonsDataFrame
### Title: Converts ternary*-class objects to SpatialPolygonsDataFrame
### Aliases: ternary2SpatialPolygonsDataFrame
###   ternary2SpatialPolygonsDataFrame.ternaryPolygons

### ** Examples

#   See ?createTernaryGrid for an example



cleanEx()
nameEx("ternaryClasses-methods")
### * ternaryClasses-methods

flush(stderr()); flush(stdout())

### Name: ternaryClasses
### Title: Extract the ternary classification (class polygons) from a
###   ternarySystem-object
### Aliases: ternaryClasses ternaryClasses.character
###   ternaryClasses.ternarySystem

### ** Examples


library( "ternaryplot" ) 

#   Fetch the definition of a ternary system
s <- ternaryPlot( s = "dummy" )

#   Create the ternary grid and convert it to 
#   sp SpatialPolygonsDataFrame
tc   <- ternaryClasses( s )

#   Plot the ternary system and the grid
ternaryPlot( s = s )
ternaryPlot( s = tc, add = FALSE, 
    polygonExtra = list( border = "red", lty = 2 ) ) 

# #   Alternatively:
tcSp <- ternary2SpatialPolygonsDataFrame( tc ) 
# sp::plot( tcSp, add = TRUE, border = "darkred", lty = 3 ) 



cleanEx()
nameEx("ternaryPlot-methods")
### * ternaryPlot-methods

flush(stderr()); flush(stdout())

### Name: ternaryPlot
### Title: Generic ternary-data plotting
### Aliases: ternaryPlot ternaryPlot.character ternaryPlot.ternaryPolygons
###   ternaryPlot.ternarySystem

### ** Examples


library( "ternaryplot" ) 


# Split the window in 4 sub-plots
# -------------------------------

#   Save and then set graphical parameters
op <- par( no.readonly = TRUE ) 
par( mfrow = c(2,2), cex = .5 )

# par( mar = c(3,2,0,2)+.1 ) # for larger triangles


# Default ternary plot
# ====================

s <- ternaryPlot() 


# Change axis orientation (clockwise vs counter-clockwise)
# ========================================================

# Set new orientation
blrClock( s ) <- rep( FALSE, 3 ) 

# Plot
ternaryPlot(s) 


# Change axis orientation and frame angles
# ========================================

# Set new orientation and angles
blrClock( s )  <- c( FALSE, TRUE, NA ) 
tlrAngles( s ) <- c( 45, 90, 45 ) 

# Plot
ternaryPlot(s) 


# Same, other direction
# =====================

blrClock( s ) <- c( TRUE, NA, FALSE ) 
tlrAngles( s ) <- c(45,45,90) 

ternaryPlot(s) 


# reset par
par( op )


# Custom colors, margins and arrow breaks
# =======================================

#   Set general graphical parameters (enlarged plot)
par( fg = "darkred", col.lab = "darkblue", mar = c(3,2,0,2)+.1 ) 
    # mar = c(bottom, left, top, right) 

#   Set specific graphical parameters
tpPar( grid.line.col = "white", arrowsBreak = FALSE, 
    plot.bg = gray( .95 ) )

ternaryPlot()


# reset par
par( op )
tpPar( reset = TRUE )



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ternaryplot-package")
### * ternaryplot-package

flush(stderr()); flush(stdout())

### Name: ternaryplot-package
### Title: Ternary plots and ternary classifications
### Aliases: ternaryplot-package
### Keywords: package

### ** Examples

# Examples coming later



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
