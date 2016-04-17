
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryPlot ==============================================

#'Plot a ternary diagram, ternary data and ternary classifications.
#'
#'Plot a ternary diagram, ternary data and ternary 
#'  classifications.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}}, or 
#'    \item A single character string, the name of an existing 
#'      pre-defined \code{\link[ternaryplot]{ternarySystem-class}}.
#'  } 
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing point ternary data 
#'  to be plot on the graph. It should contain the 3 columns 
#'  names given by \code{blrNames(s)}. If missing, only the 
#'  ternary classification is drawn.
#'
#'@param type 
#'  Single character string. Type of plot desired. The 
#'  following values are possible: \code{"p"} for points 
#'  and \code{"n"} for nothing (just a base ternary plot, 
#'  without data overlay). See also 
#'  \code{\link[ternaryplot]{ternaryPoints}}.
#'
#'@param main 
#'  Single character string, or \code{\link[base]{expression}}. 
#'  Main title of the plot. Passed on to 
#'  \code{\link[graphics]{title}}. If \code{NULL}, the default 
#'  title defined in \code{s} is used (i.e. \code{main} 
#'  overrides any title settings in \code{s}). To suppress 
#'  the title set to \code{NA} or \code{""}.
#'
#'@param sub 
#'  Single character string, or \code{\link[base]{expression}}. 
#'  Subtitle of the plot. Passed on to 
#'  \code{\link[graphics]{title}}. Set to \code{NULL}, 
#'  \code{NA} or \code{""} to suppress the subtitle. Notice 
#'  that when used in conjunction with 
#'  \code{\link[ternaryplot]{ternaryStyle}}\code{(margin=TRUE)}, 
#'  \code{sub} is drawn below the figure margins (so don't used
#'  it in that case).
#'
#'@param axes 
#'  Single logical value. If \code{TRUE}, axis are drawn on 
#'  the plot. See also \code{\link[ternaryplot]{ternaryAxis}}.
#'  By axis is meant the axis lines, the axis tick lines, 
#'  the axis tick labels, the arrows and the axis titles. 
#'  See \code{arrows} and \code{arrowsBreak} in 
#'  \code{\link[ternaryplot]{tpPar}} for suppressing the 
#'  axis arrows or the axis arrows 'break' (straight arrows), 
#'  respectively.
#'
#'@param classes 
#'  Single logical value. If \code{TRUE}, the polygons of the 
#'  classification system defined in \code{s} is drawn on 
#'  the plot (with labels inside the polygons). Only relevant 
#'  if \code{s} does have a classification system.
#'
#'@param grid 
#'  Single logical value. If \code{TRUE}, a grid is drawn 
#'  behind the ternary plot.
#'
#'@param frame.plot 
#'  Single logical value. If \code{TRUE}, a frame is drawn 
#'  around the plot, as well as a background colour (when 
#'  relevant). See also 'plot.bg' in 
#'  \code{\link[ternaryplot]{tpPar}} and 
#'  \code{\link[ternaryplot]{ternaryBox}}. Notice that 
#'  \code{\link[graphics]{par}}\code{(bty="n")} overrides 
#'  \code{frame.plot} (no box is drawn).
#'
#'@param scale 
#'  Either a logical value or a \code{\link[base]{data.frame}} with 
#'  3 columns and 2 rows. If \code{TRUE}, the triangle plot will 
#'  be scaled to fit the data. If \code{FALSE}, no scaling is 
#'  performed, and the full extent triangle plot is drawn. If a 
#'  \code{data.frame}, contains the min and max limits of each 
#'  of the 3 variables (columns = variables, rows = min and max).
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryPoints}}
#'
#'
#'@example inst/examples/ternaryPlot-example.R
#'
#'@rdname ternaryPlot-methods
#'
#'@export 
#'
ternaryPlot <- function( 
 s, 
 ... 
){  
    if( missing( s ) ){ 
        UseMethod( "ternaryPlot", object = character(0) ) 
    }else{ 
        UseMethod( "ternaryPlot" ) 
    }   
}   



#'@rdname ternaryPlot-methods
#'
#'@method ternaryPlot character
#' 
#'@export
#'
ternaryPlot.character <- function(
 s, 
 ... 
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    ternaryPlot.ternarySystem( s = s, ... ) 
}   



#'@rdname ternaryPlot-methods
#'
#'@method ternaryPlot ternarySystem
#'
#'@export
#'
ternaryPlot.ternarySystem <- function( 
    s, 
    x = NULL, 
    type = "p", 
    main = NULL, 
    sub = NULL, 
    axes = TRUE, 
    classes = TRUE, 
    grid = TRUE, 
    frame.plot = TRUE,
    scale = FALSE, 
    ... 
){  
    # Plot something:
    ternaryWindow.ternarySystem( s = s ) 
    
    
    oldTpPar <- tpPar() # Backup parameters
    .par     <- par()
    
    
    #   Draw a box without borders around the plot
    if( frame.plot & (.par[[ "bty" ]] != "n") ){
        tpPar( "axis.line.col" = NA ) # No axis line
        
        ternaryBox.ternarySystem( 
            s  = s, 
            bg = oldTpPar[[ "plot.bg" ]] ) 
        
        #   Restore parameters
        tpPar( "axis.line.col" = oldTpPar[[ "axis.line.col" ]] ) 
    }   
    
    #   Draw the ternary classes *background*, when relevant
    if( classes & (nrow( s[[ "classes" ]] ) > 0) ){
        if( !all( is.na( oldTpPar[[ "class.bg" ]] ) ) ){
            ternaryPolygons.ternarySystem( 
                s      = s, 
                bg     = oldTpPar[[ "class.bg" ]], 
                border = NA, 
                labels = NA ) 
        }   
    }   
    
    
    #   Add the grid
    if( grid ){
        ternaryGrid.ternarySystem( s = s ) 
    }   
    
    
    #   Plot any ternary classification:
    if( classes & (nrow( s[[ "classes" ]] ) > 0) ){
        # The background was already plot
        ternaryPolygons.ternarySystem( s = s, bg = NA ) 
    }   
    
    
    #   Plot any ternary points in x
    if( is.null( x ) ){ x <- data.frame() } 
    
    if( (nrow( x ) > 0) & (type == "p") ){ 
        ternaryPoints.ternarySystem( s = s, x = x, ... )
        
        #   If the triangle is undetermined, attribute to 
        #   the bottom-left-right variables the names of the 
        #   first 3 variables in x
        # s <- .fixTernarySystem( s = s, x = x ) 
    }   
    
    
    #   Add the axis and the box overlay
    if( axes ){
        ternaryAxis.ternarySystem( s = s ) 
    }   
    
    
    if( frame.plot & (.par[[ "bty" ]] != "n") ){
        ternaryBox.ternarySystem( s = s ) 
    }   
    
    
    #   Main and sub-titles
    if( is.null( main ) ){
        main <- s[[ "main" ]] 
    }   
    
    if( any( c( !is.null( main ), !is.null( main ) ) ) ){
        title( main = main, sub = sub )
    }   
    
    
    return( invisible( s ) ) 
}   



# .ternaryLims ===================================================

#'INTERNAL: Find optimal axis limits for a ternary plot.
#'
#'INTERNAL: Find optimal axis limits for a ternary plot.
#'
#'
#'@param s 
#'  Either a character string naming the ternary classification 
#'  system to be used (if pre-defined) or a  
#'  \code{\link[ternaryplot]{ternarySystem}}, instead of \code{x}.
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a \code{\link[base]{matrix}} 
#'  containing point ternary data (x-y-x) to be ploted on the graph. 
#'  It should contain the 3 columns names given in \code{s};
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a \code{\link[base]{data.frame}} with 3 columns (bottom, 
#'  left and right variables) and 2 rows: the minimun value and the 
#'  maximum value of each variable.
#'
#'
#'@rdname ternaryLims-methods
#'
#'@export 
#'
#'@keywords internal
#'
.ternaryLims <- function( 
    s, 
    ... 
){  
    if( missing( s ) ){ 
        UseMethod( ".ternaryLims", object = character(0) ) 
    }else{ 
        UseMethod( ".ternaryLims" ) 
    }   
}   



#'@rdname ternaryLims-methods
#'
#'@method .ternaryLims character
#'
#'@export
#'
.ternaryLims.character <- function(
 s, 
 ... 
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    .ternaryLims( s = s, ... ) 
}   



#'@rdname ternaryLims-methods
#'
#'@method .ternaryLims ternarySystem
#'
#'@export
#'
.ternaryLims.ternarySystem <- function( 
 s, 
 x, 
 ... 
){  
    # Fetch the variable names
    .blrNames <- blrNames.ternarySystem( s = s )  
    
    # Extract the variables
    x <- x[, .blrNames ] 
    
    # x <- data.frame( "B" = runif(10,1,2), "L" = runif(10,1,2), "R" = runif(10,1,2) )
    
    # Find the minimum and maximum of each of the 3 axis
    blrMin <- apply( X = x, MARGIN = 2, FUN = min ) 
    blrMax <- apply( X = x, MARGIN = 2, FUN = max ) 
    
    # Round the values to the nearest upper / lower value
    # NB: only works because values are positive
    blrMin <- floor( x = blrMin * 10 ) / 10 
    blrMax <- ceiling( x = blrMax * 10 ) / 10 
    
    # Find the range of values
    blrRange <- blrMin - blrMax
    
    # Difference between axis range and max axis range
    blrRangeDiff <- max(blrRange) - blrRange 
    
    # Correct the min and max to obtain equilateral triangle
    blrMin <- blrMin - blrRangeDiff 
    blrMax <- blrMax + blrRangeDiff 
    rm( blrRange, blrRangeDiff ) 
    
    blrLims <- as.data.frame( rbind( blrMin, blrMax ) )
    rownames( blrLims ) <- c("min","max") 
    colnames( blrLims ) <- .blrNames 
    
    # all( unlist( lapply( X = 1:3, FUN = function(X){ all(x[,X] >= blrLims[1,X]) } ) ) )  
    # all( unlist( lapply( X = 1:3, FUN = function(X){ all(x[,X] <= blrLims[2,X]) } ) ) ) 
    
    return( blrLims )
}   


