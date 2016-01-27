
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# ternary2SpatialPolygons ==============================

#'Converts ternary*-class objects to SpatialPolygons
#'
#'Converts ternary*-class objects to \code{\link[sp]{SpatialPolygons}}
#'
#'
#'@param x 
#'  Either a \code{ternaryPolygons} object (such as created by 
#'  \code{\link[ternaryplot]{ternaryClasses}} or 
#'  \code{\link[ternaryplot]{createTernaryGrid}}), or a \code{ternarySystem} 
#'  (such as obtained with \code{\link[ternaryplot]{getTernarySystem}}), or 
#'  a single character string naming a \code{ternarySystem} that can be 
#'  fetched using \code{\link[ternaryplot]{getTernarySystem}}.
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A \code{\link[sp]{SpatialPolygons}}
#'
#'
#'@rdname ternary2SpatialPolygons-methods
#'
#'@example inst/examples/ternary2SpatialPolygons-example.R
#'
#'@export 
#'
ternary2SpatialPolygons <- function(
 x, 
 ... 
){  
    UseMethod( "ternary2SpatialPolygons" ) 
}   


#'@rdname ternary2SpatialPolygons-methods
#'
#'@method ternary2SpatialPolygons ternaryPolygons
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPolygons}{ternaryPolygons}( x, ... ) 
#'
#'@importFrom sp Polygons 
#'@importFrom sp Polygon 
#'@importFrom sp SpatialPolygons 
ternary2SpatialPolygons.ternaryPolygons <- function(
 x, 
 ... 
){  
    s  <- ternarySystem( x = x )  
    # x  <- x[[ "grid" ]] 
    
    if( nrow( x ) > 0 ){
        # x  <- x[ order( x[, "id"] ), ] 
        
        idCol <- attr( x = x, which = "idCol" )
        
        id <- x[, idCol ] 
        x  <- x[, colnames( x ) != idCol ] 
        # x <- subset( x, select = eval( quote( -id ) ) )
        
        .blrNames <- blrNames( s ) 
        
        #   Transform from Top-Left-Right to X-Y
        xy <- ternary2xy( s = s, x = x[, .blrNames ] ) 
        
        #   Factor version of IDs
        idf <- factor( x = id, levels = unique( id ), 
            labels = as.character( unique( id ) ) ) 
        
        xy  <- split( x = xy, f = idf ) 
        nxy <- names( xy ) 
        rm( idf )
        
        # browser()
        
        pxy <- lapply( 
            X   = 1:length( xy ), 
            FUN = function(i){
                #   Check if the polygon is closed or not, 
                #   and if not, close it
                if( !all( xy[[ i ]][ 1L, ] == xy[[ i ]][ nrow( xy[[ i ]] ), ] ) ){
                    xy[[ i ]] <- rbind( xy[[ i ]], xy[[ i ]][ 1L, ] )
                }   
                
                return( sp::Polygons( srl = list( sp::Polygon( coords = xy[[ i ]] ) ), 
                    ID = nxy[ i ] ) )
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
        # pxy <- sp::SpatialPolygonsDataFrame( Sr = pxy, data = x, match.ID = FALSE )
    }else{
        warning( "No polygon to be converted (empty 'ternaryPolygons')" )
        
        pxy <- NULL 
    }   
    
    return( pxy ) 
}   


#'@rdname ternary2SpatialPolygons-methods
#'
#'@method ternary2SpatialPolygons ternarySystem
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPolygons}{ternarySystem}( x, ... ) 
#' 
ternary2SpatialPolygons.ternarySystem <- function(
 x, 
 ... 
){  
    x <- ternaryClasses( s = x ) 
    
    if( nrow( x ) > 0 ){
        return( ternary2SpatialPolygons.ternaryPolygons( x = x, ... ) ) 
    }else{
        return( NULL ) 
    }   
}   


#'@rdname ternary2SpatialPolygons-methods
#'
#'@method ternary2SpatialPolygons character
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPolygons}{character}( x, ... ) 
#' 
ternary2SpatialPolygons.character <- function(
 x, 
 ... 
){  
    s <- getTernarySystem( s = x ) 
    
    return( ternary2SpatialPolygons.ternarySystem( x = s ) ) 
}   


