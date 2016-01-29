
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryBox ====================================================

#'Draw a Box around a ternary plot
#'
#'Draw a Box around a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{box}}.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param bg 
#'  Single character string, representing a colour. Colour 
#'  of the box filling (background). Default is \code{NA}, 
#'  no fill-colour. 
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{polygon}}.
#'  You can for instance set \code{border} for the color of the 
#'  box outline, \code{col}, for the fill-color of the box, 
#'  \code{lwd} (outline thickness) or \code{lty} (line-type).
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates.
#'
#'
#'@rdname ternaryBox-methods
#'
#'@export 
#'
ternaryBox <- function( s, ... ){ 
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryBox)" ) 
    }   
    
    UseMethod( "ternaryBox" ) 
}   



# INTERNAL. Converts x-y points to SpatialPolygons.
#   The polygon is closed internally (first value added as last 
#   value)
#'@importFrom sp Polygon
#'@importFrom sp Polygons
#'@importFrom sp SpatialPolygons
.xy2SpatialPolygons <- function( xy ){ 
    if( !identical( xy[1], xy[ nrow( xy ), ] ) ){ 
        xy <- rbind( xy, xy[1,] ) 
    }   
    
    p <- sp::Polygon( coords = xy )
    p <- sp::Polygons( srl = list( p ), ID = 1L )
    p <- sp::SpatialPolygons( Srl = list( p ), pO = 1L ) 
    
    return( p ) 
}   



#'@rdname ternaryBox-methods
#'
#'@method ternaryBox ternarySystem
#'
#'@export
#' 
ternaryBox.ternarySystem <- function( 
 s, 
 bg = NA, 
 ... 
){  
    .tpPar <- tpPar()
    .par   <- par() 
    
    axis.line.lwd <- .tpPar[[ "axis.line.lwd" ]] 
    axis.line.col <- .tpPar[[ "axis.line.col" ]] 
    
    if( is.null( axis.line.lwd ) ){
        axis.line.lwd <- .par[[ "lwd" ]] 
    }   
    
    if( is.null( axis.line.col ) ){
        axis.line.col <- .par[[ "fg" ]] 
    }   
    
    scale <- s[[ 'scale' ]]
    
    ## Convert the scale into a triangular frame
    tpBox <- data.frame( 
        "B"    = c( scale[ 1, 1 ], scale[ 2, 1 ], scale[ 1, 1 ] ), 
        "L"    = c( scale[ 1, 2 ], scale[ 1, 2 ], scale[ 2, 2 ] ), 
        "R"    = c( scale[ 2, 3 ], scale[ 1, 3 ], scale[ 1, 3 ] ), 
        "row.names" = c( "left", "right", "top" ) 
    )   
    
    colnames( tpBox ) <- blrNames( s = s )   
    
    
    # Convert the scale to x-y values
    tpBox <- ternary2xy( s = s, x = tpBox ) 
    
    polygon( x = tpBox[, "x" ], y = tpBox[, "y" ], 
        lwd = axis.line.lwd, col = bg, 
        border = axis.line.col, ... )
    
    out <- tpBox[, c( "x", "y" ) ] 
    
    # if( .tpPar[[ "sp" ]] ){ out <- .xy2SpatialPolygons( xy = out ) }
    
    return( invisible( out ) ) 
}   


