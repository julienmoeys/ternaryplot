
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
#'  Package-options \code{bin.border.col} and \code{bin.bg.fun}  
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
#'@param breaks 
#'  See \code{\link[base]{cut}}. Break-values or number of 
#'  groups to be used when preparing the colour-scale and 
#'  the colour-scale legend for the binned values. See 
#'  also \code{cut.args} below.
#'
#'@param cut.args 
#'  If not \code{NULL}, a list of additional arguments to be 
#'  passed to \code{\link[base]{cut}} (see \code{breaks} 
#'  above). Values \code{x}, \code{breaks}, \code{dig.lab} 
#'  and \code{include.lowest} not allowed. \code{dig.lab} 
#'  is internally set to 0 (as counts are integer values) 
#'  and \code{include.lowest} is set to \code{TRUE} internally.
#'
#'@param legend.add 
#'  Single logical value. If \code{TRUE} (the default), 
#'  a legend is added on the plot.
#'
#'@param legend.x 
#'  See argument \code{x} in \code{\link[graphics]{legend}}. 
#'  Position of the legend. Default value is \code{topright} 
#'  unless \code{tlrAngles(s)[3L]} is higher than 60 degrees 
#'  (which indicates a ternary system with a right-angle 
#'  located at the right side of the plot), in which case 
#'  the value is set to \code{topleft} (set internally in 
#'  all cases).
#'
#'@param legend.title 
#'  See argument \code{title} in \code{\link[graphics]{legend}}. 
#'  Title of the legend.
#'
#'@param legend.args 
#'  If not \code{NULL}, a list of additional arguments to be 
#'  passed to \code{\link[graphics]{legend}}. Arguments 
#'  \code{x} and \code{title} are excluded and should be set 
#'  with \code{legend.x} and \code{legend.title}.
#'
#'@param noZero 
#'  Single logical value. If \code{TRUE} (the default), 
#'  zero-counts values are not shown on the plot. This also 
#'  makes the legend more readable.
#'
#'@param \dots 
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryPolygons}}.
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
    breaks = 6L, 
    cut.args = NULL, 
    legend.add = TRUE, 
    legend.x = NULL, 
    legend.title = "counts", 
    legend.args = NULL, 
    noZero = TRUE, 
    ... 
){  
    .tpPar <- tpPar()
    
    grd <- ternaryCount.ternarySystem( s = s, x = x, 
        grid = grid )
    
    counts <- attr( x = grd, which = "counts" )
    
    if( noZero ){
        idCol <- attr( x = grd, which = "idCol" ) 
        
        counts <- counts[ counts > 0 ]
        
        grd <- grd[ grd[, idCol ] %in% names( counts ), ]
        
        attr( x = grd, which = "counts" ) <- counts 
    }   
    
    if( length( breaks ) == 1L ){
        cRange <- range( counts, na.rm = TRUE ) 
        breaks <- round( seq( 
            from       = cRange[ 1L ],
            to         = cRange[ 2L ],
            length.out = breaks ), digits = 0L ) 
        breaks <- unique( breaks ) 
    }   
    
    cut.args0 <- list( 
        "x"              = counts, 
        "breaks"         = breaks, 
        "dig.lab"        = 0L, 
        "include.lowest" = TRUE 
    )   
    
    if( !is.null( cut.args ) ){
        if( !("list" %in% class( cut.args )) ){
            stop( "If not NULL 'cut.args' must be a list" )
        }   
        
        cut.args0 <- c( cut.args0, cut.args ) 
    }   
    
    ccounts <- do.call( what = "cut", args = cut.args0 )
    
    bin.bg.fun <- .tpPar[[ "bin.bg.fun" ]]
    
    .levels <- levels( ccounts ) 
    bg0     <- rev( bin.bg.fun( n = length( .levels ) ) ) 
    names( bg0 ) <- .levels 
    bg      <- bg0[ as.character( ccounts ) ]
    
    border <- .tpPar[[ "bin.border.col" ]] 
    
    ternaryPolygons( s = grd, bg = as.character( bg ), 
        border = border, ... )   
    
    if( legend.add ){
        if( is.null( legend.x ) ){
            if( tlrAngles( s )[ 3L ] <= 60 ){
                legend.x <- "topright"
            }else{
                legend.x <- "topleft"
            }   
        }   
        
        legend.args0 <- list( 
            "x"       = legend.x, 
            "legend"  = .levels, 
            "fill"    = bg0, 
            "title"   = legend.title 
        )   
        
        if( !is.null( legend.args ) ){
            legend.args0 <- c( legend.args0, legend.args ) 
        }   
        
        do.call( what = "legend", args = legend.args0 )
    }   
    
    return( invisible( grd ) ) 
}   

