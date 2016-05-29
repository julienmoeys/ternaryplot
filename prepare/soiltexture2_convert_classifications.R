
library( "soiltexture" )

classif <- c( "USDA.TT", "FR.AISNE.TT", "FR.GEPPA.TT", 
    "DE.BK94.TT" )

for( i in 1:length( classif ) ){
    cl <- TT.get( classif[ i ] )
    
    cat( sprintf( "%s classes:\n\n", classif[ i ] ) )
    
    .name <- unlist( lapply(
        X   = cl[[ "tt.polygons" ]], 
        FUN = function( .cl ){
            .cl[[ "name" ]] 
        }   
    ) ) 
    
    .points <- lapply(
        X   = cl[[ "tt.polygons" ]], 
        FUN = function( .cl ){
            .cl[[ "points" ]] 
        }   
    )   
    
    # names( .points ) <- NULL 
    
    classes <- data.frame( 
        "abbrev" = names( cl[[ "tt.polygons" ]] ), 
        "name" = .name, 
        "verticesId" = I( .points ), 
        stringsAsFactors = FALSE 
    )   
    
    rownames( classes ) <- NULL
    
    dput( classes )
    
    cat( "\n\n" )
    
}   


