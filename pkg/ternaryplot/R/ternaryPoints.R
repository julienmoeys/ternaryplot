
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryPoints =================================================

#'Add points to a ternary plot
#'
#'Add points to a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{points}}.
#'
#'
#'@param s  
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param x  
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing ternary data-points.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{points}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates.
#'  
#'
#'@rdname ternaryPoints-methods
#'
#'@export 
#'
ternaryPoints <- function( 
 s, 
 ... 
){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryPoints)" ) 
    }   
    
    UseMethod( "ternaryPoints" ) 
}   



#'@rdname ternaryPoints-methods
#'
#'@method ternaryPoints ternarySystem
#'
#'@export
#'
#'@importFrom sp SpatialPoints
#'@importFrom graphics points
ternaryPoints.ternarySystem <- function( 
 s, 
 x, 
 ... 
){ 
    xy <- ternary2xy.ternarySystem( s = s, x = x ) 
    
    graphics::points( x = xy[, "x" ], y = xy[, "y" ], ... ) 
    
    out <- xy[, c( "x", "y" ) ]
    
    # if( getTpPar( "sp" ) ){ out <- sp::SpatialPoints( coords = out ) }
    
    return( invisible( out ) ) 
}   


