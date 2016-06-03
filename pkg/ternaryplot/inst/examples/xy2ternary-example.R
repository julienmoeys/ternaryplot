
library( "ternaryplot" ) 

hypres <- getTernarySystem( "hypres" )

xyData <- data.frame(
    "x" = c( 0, 100,   0, 100, 50 ), 
    "y" = c( 0, 100, 100,   0, 50 ) 
)   

tData <- xy2ternary( s = hypres, data = xyData, 
    yxNames = c( "x", "y" ) )

tData

    # # Visualise the result:

    # ternaryStyle( "publication" )
    # ternaryPlot( hypres )
    # points( x = xyData[, "x" ], y = xyData[, "y" ], 
        # pch = 1, cex = 2 )
    # notNeg <- apply( X = tData, MARGIN = 1, 
        # FUN = function(x){ !any(x < 0) } )
    # ternaryPoints( s = hypres, x = tData[ notNeg, ], 
        # pch = 3 ) 
