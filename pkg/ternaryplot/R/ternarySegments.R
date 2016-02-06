
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternarySegments ===============================================

#'Draw a sequence of ternary segments on a triangle plot
#'
#'Draw a sequence of ternary segments on a triangle plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{segments}}.
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
#'  coordinates of points *from* which to draw.
#'
#'@param to
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing the ternary 
#'  coordinates of points *to* which to draw.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[graphics]{segments}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates.
#'  
#'
#'@rdname ternarySegments-methods
#'
#'@export 
#'
#'@docType methods
#'
ternarySegments <- function( 
 s, 
 from, 
 to, 
 ... 
){   
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternarySegments)" ) 
    }   
    
    UseMethod( "ternarySegments" ) 
}   



    ## # Converts x-y segments to sp::SpatialLines
    ## #@importFrom sp SpatialLines
    ## #@importFrom sp Lines
    ## #@importFrom sp Line
    # .xySegments2SpatialLines <- function( fromXY, toXY ){ 
        # coords <- data.frame( 
            # "x" = rep( NA_real_, 2 ), 
            # "y" = rep( NA_real_, 2 ) ) 
        
        # out <- sp::SpatialLines( lapply( 
            # X   = 1:nrow(fromXY), 
            # FUN = function(i){ 
                # coords[, "x" ] <- c( fromXY[ i, "x" ], toXY[ i, "x" ] ) 
                # coords[, "y" ] <- c( fromXY[ i, "y" ], toXY[ i, "y" ] ) 
                
                # return( sp::Lines( list( sp::Line( coords = coords ) ), ID = i ) )
            # }   
        # ) ) 
        
        # return( out )
    # }   



#'@rdname ternarySegments-methods
#'
#'@method ternarySegments ternarySystem
#'
#'@export
#'
ternarySegments.ternarySystem <- function( 
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
    
    # Draw the segments
    segments(
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


