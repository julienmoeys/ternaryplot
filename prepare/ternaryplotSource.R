
if( Sys.info()[["sysname"]] == "Linux" ){
    setwd( sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot/pkg/ternaryplot", 
        Sys.info()[[ "user" ]]
    ) )  
}else{
    setwd( sprintf( 
        "C:/Users/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot/pkg/ternaryplot", 
        Sys.info()[[ "user" ]] ) ) 
}   

#   Files to be sourced first (order matters)
sourceFiles <- c(
    "R/aa00-ternaryplot-package.R", 
    "R/aa01-ternaryplot-options.R", 
    "R/aa02-ternaryplot-classes.R", 
    "R/aa03-ternaryplot-classes-utility.R", 
    "R/aa04-ternarysystems.R", 
    "R/aa05-ternarydata.R", 
    "R/aa06-ternary2xy.R", 
    "R/aa07-plotUtilities.R" 
)   

#   Other files
sourceFiles2 <- list.files( path = "R", pattern = ".R" ) 
sourceFiles2 <- file.path( "R", sourceFiles2 ) 
sourceFiles2 <- sourceFiles2[ !(sourceFiles2 %in% sourceFiles) ]

for( f in c( sourceFiles, sourceFiles2 ) ){
    source( file = f )
}   


# library( "ternaryplot" )
library( "sp" )



par( mfrow = c( 2, 2 ) ) 
s1 <- ternaryPlot( "dummy" ) 
ternaryText( s1, col = "blue", what = "vertices" ) 

s2 <- ternaryPlot( "dummy2" ) 
ternaryText( s2, col = "red", what = "vertices" ) 


ternaryGeometry( s2 ) <- ternaryGeometry( s1 ) 
ternaryPlot( "dummy" ) 
ternaryPolygons( s2, border = "red", col = "red" ) 
pol <- ternaryClasses( s2 ) 
ternarySystem( pol ) <- s1 
ternaryPolygons( pol, border = "green", col = "green" ) 
ternaryText( s2, col = "red", what = "vertices" ) 

s3 <- ternaryPlot( "dummy3" ) 
ternaryText( s3, col = "red", what = "vertices" ) 



dev.off() 
ternaryPlot( "hypres" )



ternaryStyle( margin = TRUE )
ternaryPlot( "hypres" )


cols <- hcl( 
    h = 30, 
    c = seq( from = 40, to = 80, length.out = 5 ), 
    l = seq( from = 80, to = 40, length.out = 5 ) ) 
ternaryStyle( margin = TRUE )
tpPar( "class.bg" = cols ) 
tpPar( "grid.line.col" = gray( 0.8, alpha = 0.75 )  ) 
ternaryPlot( "hypres" )


# tpPar( "class.bg" = "pink" )
# ternaryPlot( "hypres" )

p <- list(
    list(
        "blrClock"  = rep( T, 3 ), 
        "tlrAngles" = rep( 60, 3 )  
    ),  
    list(
        "blrClock"  = rep( F, 3 ), 
        "tlrAngles" = rep( 60, 3 )  
    ),  
    list(
        "blrClock"  = c( F, T, NA ), 
        "tlrAngles" = c(45,90,45)  
    ),  
    list(
        "blrClock"  = c( T, NA, F ), 
        "tlrAngles" = c(45,45,90)  
    )   
)   

par( mfrow = c(2,2) )


for( i in 1:length( p ) ){
    s <- getTernarySystem() 
    
    blrClock( s )  <- p[[ i ]][[ "blrClock" ]]
    
    tlrAngles( s ) <- p[[ i ]][[ "tlrAngles" ]]
    
    ternaryPlot( s = s )
}   
#   Problem: find "## axis orientation is NA" in ternaryplot.R

par( mfrow = c(1,2), xaxs = "i", yaxs = "i" )
plot( x = 1, y = 1, asp = TRUE )
ternaryPlot( s = getTernarySystem()  )
box( col = "red", lty = 2 )



# Run some tests
s <- ternaryWindow() # Opens a plot window

# or

s <- ternaryWindow( "default" ) 

# or

s <- getTernarySystem() 
# tlrAngles(s) <- c(45,90,45) 

s <- ternaryWindow( s = s ) 
# .ternaryGridBase( s = s )

.ternaryTicks( s = s ) 
ternaryGrid( s = s ) 
ternaryBox( s = s ) 

blrClock( s ) <- rep( F, 3 ) 

# s@'ternaryGeometry'@'tlrAngles' <- c(45,90,45)

s <- ternaryWindow( s = s ) 
# .ternaryGridBase( s = s )
.ternaryTicks( s = s ) 
ternaryGrid( s = s ) 
ternaryBox( s = s ) 

blrClock( s )  <- c( F, T, NA ) 
tlrAngles( s ) <- c(45,90,45) 

s <- ternaryWindow( s = s ) 
.ternaryTicks( s = s ) 
ternaryGrid( s = s ) 
ternaryBox( s = s ) 


blrClock( s ) <- c( T, NA, F ) 
tlrAngles( s ) <- c(45,45,90)
s <- ternaryWindow( s = s ) 
.ternaryTicks( s = s ) 
ternaryGrid( s = s ) 
ternaryBox( s = s ) 
