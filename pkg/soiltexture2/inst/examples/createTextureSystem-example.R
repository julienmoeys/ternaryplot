
library( "soiltexture2" ) 

s <- createTernarySystem( 
    "ternaryVariables" = createTernaryVariables( 
        "blrNames"  = c( "SAND", "CLAY", "SILT" ), 
        "blrLabels" = c(
            "Sand 0.05 - 2 mm [%]", 
            "Clay 0 - 0.002 mm [%]", 
            "Silt 0.002 - 0.05 mm [%]" 
        ) ),  
    "ternaryGeometry" = createTernaryGeometry( ##
        "fracSum"   = 100, 
        "tlrAngles" = c( 60, 60, 60 ), # top, left, right angles
        "blrClock"  = c( TRUE, TRUE, TRUE )
    ),  
    "main" = "My texture plot" ) 

psb <- c( 0, "CLAY" = 2, "SILT" = 50, "SAND" = 2000 ) 

st <- createTextureSystem(
    "ternarySystem"          = s, 
    "particleSizeBoundaries" = psb )       

psb

#   ternaryPlot( st )
