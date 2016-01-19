
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryText ===================================================

#' Add Text to a ternary plot
#'
#' Add Text to a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{points}}.
#'
#'
#'@param s  
#'  A \code{\link[ternaryplot]{ternarySystem}} object, or a 
#'  character string naming a pre-defined \code{ternarySystem}. 
#'  If missing, set to \code{default}.
#'
#'@param x  
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing ternary data-points, 
#'  coordinates of the text strings to be added on the plot.
#'
#'@param labels  
#'  A vector of character strings, or expressions to be added 
#'  on the triangle plot. See \code{\link[graphics]{text}}.
#'
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{text}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates or 
#'  a \code{Spatial*} objects (see \code{.plot}).
#' 
#'
#'@rdname ternaryText-methods
#'
#'@export 
#'
ternaryText <- function( s, ... ){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryText)" ) 
    }   
    
    UseMethod( "ternaryText" ) 
}   



#'@rdname ternaryText-methods
#'
#'@method ternaryText ternarySystem
#'
#'@export
#'
ternaryText.ternarySystem <- function( 
 s, 
 x, 
 labels, 
 .plot = TRUE, 
 ... 
){  
    xy <- ternary2xy( x = x, s = s ) 
    
    if( .plot ){ 
        text( x = xy[,"x"], y = xy[,"y"], labels = labels, ... ) 
    }   
    
    out <- xy[, c( "x", "y" ) ]
    if( getTpPar( "sp" ) ){ out <- SpatialPoints( coords = out ) }
    
    return( invisible( out ) ) 
}   

