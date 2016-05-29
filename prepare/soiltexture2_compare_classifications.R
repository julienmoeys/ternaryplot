
library( "soiltexture" )
library( "soiltexture2" )

classif <- data.frame( 
    "soiltexture"  = c( "USDA.TT", "FR.AISNE.TT", "FR.GEPPA.TT", "DE.BK94.TT" ), 
    "soiltexture2" = c( "usda",    "aisne",       "geppa",       "bk94" ), 
    stringsAsFactors = FALSE 
)   

dev.off()

for( i in 1:nrow( classif ) ){
    dev.new()
    
    par( mfrow = c( 1, 2 ) ) 
    
    TT.plot( class.sys = classif[ i, "soiltexture" ] )
    
    ternaryStyle( "publication" )
    
    ternaryPlot( s = classif[ i, "soiltexture2" ] )
    
    tpPar( reset = TRUE )
}   
