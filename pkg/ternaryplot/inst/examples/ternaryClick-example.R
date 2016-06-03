
library( "ternaryplot" ) 

# Test ternaryClick with different ternary geometries
# ==========================================================

#   Number of clicks
n <- 2L

if( interactive() ){
    
    hypres <- getTernarySystem( "hypres" )
    
    blrClock( hypres )
    
    blrClock0 <- list(
        "TTT" = c(  TRUE,  TRUE,  TRUE ), 
        "FFF" = c( FALSE, FALSE, FALSE ), 
        "FTX" = c( FALSE,  TRUE,    NA ), 
        "TXF" = c(  TRUE,    NA, FALSE ) 
    )   
    
    ternaryStyle( "publication" )
    
    for( i in 1:length( blrClock0 ) ){
        message( paste( 
            ":::", 
            paste( blrClock0[[ i ]], collapse = ", " ), 
            ":::" 
        ) )  
        
        blrClock( hypres ) <- blrClock0[[ i ]] 
        
        ternaryPlot( hypres ) 
        
        print( ternaryClick( hypres, n = n, type = "p" ) ) 
        
        # flush.console()
    }   
}   

tpPar( reset = TRUE )
