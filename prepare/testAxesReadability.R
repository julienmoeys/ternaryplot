
library( "ternaryplot" ) 

dat    <- data.frame( "clay" = 10, "silt" = 30, "sand" = 60 )

s0 <- getTernarySystem( "default" )

blrNames( s0 )  <- c( "sand", "clay", "silt" )
blrLabels( s0 ) <- blrNames( s0 ) 



# Plot 1: with axis ticks, labels and arrows
# ----------------------------------------------------------

par( mfrow = c( 2, 2 ) )

ternaryStyle( "publication", margin = TRUE )

tpPar( arrowsBreak = FALSE )

ternaryPlot( s = s0, x = dat ) 

s <- s0 
blrClock( s ) <- rep( FALSE, 3 )

ternaryPlot( s = s, x = dat ) 

s <- s0 
blrClock( s ) <- c( FALSE, TRUE, NA ) 

ternaryPlot( s = s, x = dat ) 

s <- s0 
blrClock( s ) <- c( TRUE, NA, FALSE ) 

ternaryPlot( s = s, x = dat ) 



# Plot 1: with axis ticks, no arrows, and labels on submits
# ----------------------------------------------------------

plotVertex <- function(s){
    vertex <- data.frame( 
        "clay" = c( 100,   0,   0 ), 
        "silt" = c(   0, 100,   0 ), 
        "sand" = c(   0,   0, 100 ), 
        row.names = c( "100%\nclay", "100%\nsilt", "100%\nsand" ) )
    
    for( i in 1:nrow(vertex) ){
        .par <- par( "xpd" = TRUE )
        
        ternaryText( s = s, x = vertex[i,], 
            labels = rownames( vertex )[ i ] )
        
        par( "xpd" = .par[[ "xpd" ]] )
    }   
}   

ternaryStyle( "publication" )

par( mfrow = c( 2, 2 ), oma = c( 0, 0, 2, 0 ), 
    mar = c(3, 3, 0, 3) + 0.1 ) #  c(bottom, left, top, right)
tpPar( ticksAt = seq( from = 0.2, to = 0.8, by = 0.2 ), 
    arrows = FALSE )

ternaryPlot( s = s0, x = dat ) 
plotVertex( s = s0 )

title( 
    main = "How readable is your ternary plot?", 
    outer = TRUE )

s <- s0 
blrClock( s ) <- rep( FALSE, 3 )

ternaryPlot( s = s, x = dat ) 
plotVertex( s = s )

s <- s0 
blrClock( s ) <- c( FALSE, TRUE, NA ) 

ternaryPlot( s = s, x = dat ) 
plotVertex( s = s )

s <- s0 
blrClock( s ) <- c( TRUE, NA, FALSE ) 

ternaryPlot( s = s, x = dat ) 
plotVertex( s = s )
