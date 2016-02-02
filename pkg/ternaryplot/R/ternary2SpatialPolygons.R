
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
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternaryPolygons-class}} 
#'      object, or 
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A single character string naming an existing  
#'      \code{\link[ternaryplot]{ternaryVariables-class}}.
#'  }  
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A \code{\link[sp]{SpatialPolygons}} object.
#'
#'
#'@rdname ternary2SpatialPolygons-methods
#'
#'@example inst/examples/ternary2SpatialPolygons-example.R
#'
#'@export 
#'
ternary2SpatialPolygons <- function(
 s, 
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
#'@importFrom sp Polygons 
#'@importFrom sp Polygon 
#'@importFrom sp SpatialPolygons 
ternary2SpatialPolygons.ternaryPolygons <- function(
 s, 
 ... 
){  
    s0  <- ternarySystem( x = s )  
    # s  <- s[[ "grid" ]] 
    
    if( nrow( s ) > 0 ){
        # s  <- s[ order( s[, "id"] ), ] 
        
        idCol <- attr( x = s, which = "idCol" )
        
        id <- s[, idCol ] 
        s  <- s[, colnames( s ) != idCol ] 
        # s <- subset( s, select = eval( quote( -id ) ) )
        
        .blrNames <- blrNames( s0 ) 
        
        #   Transform from Top-Left-Right to X-Y
        xy <- ternary2xy( s = s0, x = s[, .blrNames ] ) 
        
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
        
        # s <- aggregate( s, by = list( "ID" = id ), FUN = mean ) 
        # rownames( s ) <- s[, "ID" ] 
        # s <- subset( s, select = eval( quote( -ID ) ) )
        
        # pxy <- Polygons( srl = pxy, ID = nxy ) 
        
        pxy <- sp::SpatialPolygons( Srl = pxy )
        
        # pxy <- sp::SpatialPolygonsDataFrame( Sr = pxy, data = s, match.ID = FALSE )
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
ternary2SpatialPolygons.ternarySystem <- function(
 s, 
 ... 
){  
    s <- ternaryClasses( s = s ) 
    
    if( nrow( s ) > 0 ){
        return( ternary2SpatialPolygons.ternaryPolygons( s = s, ... ) ) 
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
ternary2SpatialPolygons.character <- function(
 s, 
 ... 
){  
    s <- getTernarySystem( s = s ) 
    
    return( ternary2SpatialPolygons.ternarySystem( s = s ) ) 
}   


