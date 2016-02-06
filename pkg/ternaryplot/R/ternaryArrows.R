
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryArrows =================================================

#'Add Arrows to a ternary plot
#'
#'Add Arrows to a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{arrows}}.
#'
#'
#'@param s  
#'  A \code{\link[ternaryplot]{ternarySystem}} object, or a 
#'  character string naming a pre-defined \code{ternarySystem}. 
#'  If missing, set to \code{default}.
#'
#'@param from 
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing the ternary 
#'  coordinates of points *from* which to draw the arrows.
#'  Each row is an arrow, and the columns must correspond 
#'  to \code{blrNames(s)} (variable names for the bottom, 
#'  left and right axis).
#'
#'@param to
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing the ternary 
#'  coordinates of points *to* which to draw the arrows.
#'  Each row is an arrow, and the columns must correspond 
#'  to \code{blrNames(s)} (variable names for the bottom, 
#'  left and right axis).
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{arrows}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates.
#' 
#' 
#'@rdname ternaryArrows-methods
#'
#'@export 
#'
ternaryArrows <- function( 
 s, 
 from, 
 to, 
 ... 
){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryArrows)" ) 
    }   
    
    UseMethod( "ternaryArrows" ) 
}   



#'@rdname ternaryArrows-methods
#'
#'@method ternaryArrows ternarySystem
#'
#'@export
#'
ternaryArrows.ternarySystem <- function( 
 s, 
 from, 
 to, 
 ... 
){  
    # Check to and from
    if( missing( to ) ){ 
        stop( "'to' is missing" )
    }   
    
    if( class(from) != class(to) ){ 
        stop( "'from' and 'to' must be of the same class (data.frame or matrix)" )
    }           
    
    if( nrow(from) != nrow(to) ){ 
        stop( "'from' and 'to must have the same number of rows" )
    }    
    
    # Fetch coordinate columns:
    .blrNames <- blrNames.ternarySystem( s = s ) 
    from <- from[, .blrNames ]  
    to   <- to[, .blrNames ]   
    
    # Transform the coordinates into x-y values
    fromXY <- ternary2xy.ternarySystem( s = s, x = from ) 
    toXY   <- ternary2xy.ternarySystem( s = s, x = to ) 
    
    # Draw the arrows
    arrows(
        x0  = fromXY[, "x" ], 
        y0  = fromXY[, "y" ], 
        x1  = toXY[, "x" ], 
        y1  = toXY[, "y" ], 
        ...
    )   
    
    out <- list( "from" = fromXY, "to" = toXY ) 
    
    # if( getTpPar( "sp" ) ){ 
        # out <- .xySegments2SpatialLines( fromXY = fromXY, toXY = toXY ) 
    # }   
    
    return( invisible( out ) ) 
}   


