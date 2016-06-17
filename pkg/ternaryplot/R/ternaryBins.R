
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryBins ===============================================

#'Add bins (counts in ternary grid cells) to a ternary plot
#'
#'Add bins (counts in ternary grid cells) to a ternary plot
#'
#'
#'@seealso
#'  \code{\link[ternaryplot]{ternaryPlot}} for plotting 
#'  ternary bins as a background of a ternary plot, and 
#'  \code{\link[ternaryplot]{ternaryCount}} for producing 
#'  the bins and count per bins but not plotting them.
#'
#'  Package-options \code{ternaryPolygons.border.col} and \code{ternaryPolygons.bg.fun}  
#'  in \code{\link[ternaryplot]{tpPar}} are used internally.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}}. 
#'      That \code{ternarySystem-class} must contain 
#'      a ternary classification. 
#'    \item A single character string, the name of an existing 
#'      pre-defined \code{\link[ternaryplot]{ternarySystem-class}}.
#'    \item A \code{\link[ternaryplot]{ternaryPolygons-class}}.
#'  } 
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing point ternary data 
#'  to be binned and counted. It should contain the 3 columns 
#'  names given by \code{blrNames(s)}.
#'
#'@param grid 
#'  Single logical value. Set to \code{TRUE} (the default) 
#'  to retrieve counts for a systematic ternary grid, and 
#'  to \code{FALSE} to count the number of \code{x}-data 
#'  points per class in \code{s} instead.
#'
#'@param noZero 
#'  Single logical value. If \code{TRUE} (the default), 
#'  zero-counts values are not shown on the plot. This also 
#'  makes the legend more readable.
#'
#'@param \dots 
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryPolygons}}. See in particular 
#'  
#'
#'
#'@return 
#'  Invisibly returns the output of 
#'  \code{\link[ternaryplot]{ternaryCount}}.
#'
#'
#'@rdname ternaryBins-methods
#'
#'@example inst/examples/ternaryBins-example.R
#'
#'@export 
#'
ternaryBins <- function( 
    s, 
    x, 
    ... 
){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryBins)" ) 
    }   
    
    if( missing( x ) ){ 
        stop( "'x' is missing. No data to be binned and counted" ) 
    }   
    
    UseMethod( "ternaryBins" ) 
}   


#'@rdname ternaryBins-methods
#'
#'@method ternaryBins ternarySystem
#'
#'@export
#'
#'@importFrom sp SpatialPoints
ternaryBins.ternarySystem <- function( 
    s, 
    x, 
    grid = TRUE, 
    noZero = TRUE, 
    ... 
){  
    grd <- ternaryCount.ternarySystem( s = s, x = x, 
        grid = grid )
    
    
    ternaryPolygons( s = grd, z = "counts", noZero = noZero, ... )   
    
    
    return( invisible( grd ) ) 
}   

