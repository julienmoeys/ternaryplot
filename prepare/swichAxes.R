
library( "ternaryplot" ) 

dat    <- data.frame( "CLAY" = 10, "SILT" = 30, "SAND" = 60 )

s0 <- getTernarySystem( "hypres" )
blrLabels( s0 ) <- tolower( blrNames( s0 ) )

allComb <- list(
    c( "SAND", "CLAY", "SILT" ), 
    c( "SILT", "CLAY", "SAND" ), 
    c( "SAND", "SILT", "CLAY" ), 
    c( "SILT", "SAND", "CLAY" ), 
    c( "CLAY", "SAND", "SILT" ), 
    c( "CLAY", "SILT", "SAND" ) 
)   

par( mfrow = c( 3, 2 ) )

tpPar( arrowsBreak = FALSE )

ternaryStyle( "publication", margin = TRUE )

for( i in 1:length( allComb ) ){
    s <- s0 
    blrNames( s, reorder = TRUE ) <- allComb[[ i ]] 
    
    ternaryPlot( s = s, x = dat, main = NA ) 
}   
