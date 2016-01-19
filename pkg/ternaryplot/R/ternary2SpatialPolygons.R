
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# ternary2SpatialPolygonsDataFrame ==============================

#'Converts ternary*-class objects to SpatialPolygonsDataFrame
#'
#'Converts ternary*-class objects to \code{\link[sp]{SpatialPolygonsDataFrame}}
#'
#'
#'@param x 
#'  A ternary*-class object.
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A \code{\link[sp]{SpatialPolygonsDataFrame}}
#'
#'
#'@rdname ternary2SpatialPolygonsDataFrame-methods
#'
#'@example inst/examples/ternary2SpatialPolygonsDataFrame-example.R
#'
#'@export 
#'
ternary2SpatialPolygonsDataFrame <- function(
 x, 
 ... 
){  
    UseMethod( "ternary2SpatialPolygonsDataFrame" ) 
}   



#'@rdname ternary2SpatialPolygonsDataFrame-methods
#'
#'@method ternary2SpatialPolygonsDataFrame ternaryPolygons
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPolygonsDataFrame}{ternaryPolygons}( x, ... ) 
#'
#'@importFrom sp Polygons 
#'@importFrom sp Polygon 
#'@importFrom sp SpatialPolygons 
#'@importFrom sp SpatialPolygonsDataFrame 
ternary2SpatialPolygonsDataFrame.ternaryPolygons <- function(
 x, 
 ... 
){  
    s  <- ternarySystem( x = x )  
    x  <- x[[ "grid" ]] 
    
    if( nrow( x ) > 0 ){
        x  <- x[ order( x[, "id"] ), ] 
        id <- x[, "id" ] 
        x <- subset( x, select = eval( quote( -id ) ) )
        
        .blrNames <- blrNames( s ) 
        
        #   Transform from Top-Left-Right to X-Y
        xy <- ternary2xy( s = s, x = x[, .blrNames ] ) 
        
        xy  <- split( x = xy, f = as.factor( id ) ) 
        nxy <- names( xy ) 
        
        # browser()
        
        pxy <- lapply( 
            X   = 1:length( xy ), 
            FUN = function(X){ 
                sp::Polygons( srl = list( sp::Polygon( coords = xy[[ X ]] ) ), 
                    ID = nxy[ X ] )
            }   
        )   
        # pxy <- do.call( "what" = "rbind", "args" = pxy )
        
        
        #   Aggregate x
        #   TO DO: replace this by centroid calculation + xy2ternary() 
        
        x <- aggregate( x, by = list( "ID" = id ), FUN = mean ) 
        rownames( x ) <- x[, "ID" ] 
        x <- subset( x, select = eval( quote( -ID ) ) )
        
        # pxy <- Polygons( srl = pxy, ID = nxy ) 
        pxy <- sp::SpatialPolygons( Srl = pxy )
        pxy <- sp::SpatialPolygonsDataFrame( Sr = pxy, data = x, match.ID = FALSE )
    }else{
        warning( "No polygon to be converted (empty 'ternaryPolygons')" )
        
        pxy <- NULL 
    }   
    
    return( pxy ) 
}   

    # tg <- createTernaryGrid("default")
    # library( "sp" ) 
    # tgSp <- ternary2SpatialPolygonsDataFrame( tg )
    
    # plot( tgSp ) 


