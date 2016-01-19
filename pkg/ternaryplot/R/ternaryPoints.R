
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
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{points}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates or 
#'  a \code{Spatial*} objects (see \code{.plot}).
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
ternaryPoints.ternarySystem <- function( 
 s, 
 x, 
 .plot = TRUE, 
 ... 
){ 
    xy <- ternary2xy( s = s, x = x ) 
    
    if( .plot ){
        points( x = xy[, "x" ], y = xy[, "y" ], ... ) 
    }   
    
    out <- xy[, c( "x", "y" ) ]
    if( getTpPar( "sp" ) ){ out <- sp::SpatialPoints( coords = out ) }
    
    return( invisible( out ) ) 
}   


