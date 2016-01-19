
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryPlot ===================================================

#'Generic ternary-data plotting
#'
#'Generic ternary-data plotting
#'
#'
#'@param s 
#'  Either a character string naming the ternary classification 
#'  system to be used (if pre-defined), 
#'  or a \code{\link[ternaryplot]{ternarySystem}}-object, 
#'  or a \code{ternaryPolygons}-object 
#'  (such as created with \code{\link[ternaryplot]{createTernaryGrid}}).
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a \code{\link[base]{matrix}} 
#'  containing point ternary data (x-y-x) to be ploted on the graph. 
#'  It should contain the 3 columns names given in \code{s}. If 
#'  missing, only the ternary classification is drawn.
#'
#'@param scale 
#'  Either a logical value or a \code{\link[base]{data.frame}} with 
#'  3 columns and 2 rows. If \code{TRUE}, the triangle plot will 
#'  be scaled to fit the data. If \code{FALSE}, no scaling is 
#'  performed, and the full extent triangle plot is drawn. If a 
#'  \code{data.frame}, contains the min and max limits of each 
#'  of the 3 variables (columns = variables, rows = min and max).
#'
#'@param add 
#'  Single logical value. If \code{TRUE} (an existing ternary 
#'  plot has already been plotted), the base ternary plot 
#'  is not plotted again (windows, grid, axis, ...), and only 
#'  overlay data is shown (such as point data if \code{x} is 
#'  not null, or polygons overlay if \code{s} is a 
#'  \code{ternaryPolygons}-object).
#'
#'@param polygonExtra  
#'  If \code{s} is a 
#'  \code{ternaryPolygons}-object, 
#'  list of additional arguments passed to 
#'  \code{\link[graphics]{polygon}}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
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
    
    ternaryPlot( s = s, ... ) 
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
 scale = FALSE, 
 ... 
){   
    testRange <- getTpPar( par = "testRange" ) 
    testSum   <- getTpPar( par = "testSum" ) 
    
    
    # Plot something:
    ternaryWindow( s = s ) 
    ternaryBox( s = s, col = getTpPar( "plot.bg" ) ) 
    ternaryGrid( s = s ) 
    
    
    #   Extract any ternary classification:
    if( nrow( s[[ "classes" ]] ) > 0 ){
        tc <- ternaryClasses( s = s ) 
        
        class.line.col <- getTpPar( "class.line.col" ) 
        class.bg       <- getTpPar( "class.bg" ) 
        class.line.lwd <- getTpPar( "class.line.lwd" ) 
        
        ternaryPlot( s = tc, scale = scale, add = TRUE, 
            polygonExtra = list( 
                "border" = class.line.col, 
                "col"    = class.bg, 
                "lwd"    = class.line.lwd ) ) 
    }   
    
    
    
    if( is.null( x ) ){ x <- data.frame() } 
    if( nrow( x ) >= 1 ){ 
        message( "Method (data.frame,ternarySystem) not implemented yet" ) 
    }   
    
    
    #   Add the axis and the box overlay
    ternaryAxis( s = s ) 
    ternaryBox( s = s ) 
    
    return( invisible( s ) ) 
}   



#'@rdname ternaryPlot-methods
#'
#'@method ternaryPlot ternaryPolygons
#'
#'@export
#'
ternaryPlot.ternaryPolygons <- function( 
 s, 
 scale = FALSE, 
 add = FALSE, 
 polygonExtra = NULL, 
 ... 
){  
    terSys  <- ternarySystem( x = s )  
    
    if( !add ){
        ternaryPlot( s = terSys, scale = scale, ... )
    }   
    
    x  <- s[[ "grid" ]] 
    
    if( nrow( x ) > 0 ){
        x  <- x[ order( x[, "id"] ), ] 
        id <- x[, "id" ] 
        x <- subset( x, select = eval( quote( -id ) ) )
        
        .blrNames <- blrNames( terSys ) 
        
        #   Transform from Top-Left-Right to X-Y
        xy <- ternary2xy( s = terSys, x = x[, .blrNames ] ) 
        
        xy  <- split( x = xy, f = as.factor( id ) ) 
        nxy <- names( xy ) 
        
        if( !is.null( polygonExtra ) ){
            if( !is.list( polygonExtra ) ){
                stop( "'polygonExtra' must be a list (with names)" )
            }   
        }   
        
        silent <- lapply( 
            X   = 1:length( xy ), 
            FUN = function(X){ 
                argz <- list(
                    x = xy[[ X ]][, "x" ], 
                    y = xy[[ X ]][, "y" ]  
                )   
                
                argz <- c( argz, polygonExtra ) 
                
                do.call( what = "polygon", args = argz ) 
            }   
        )   
    }else{
        warning( "No polygon to be plotted (empty 'ternaryPolygons')" )
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
    .blrNames <- blrNames( s = s )  
    
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




# .ternaryClockSwitch ============================================ 

    # ## #INTERNAL. Fetch a pre-defined ternary classification system
    # ## #
    # ## #INTERNAL. Fetch a pre-defined ternary classification system
    # ## #
    # ## #
    # ## #@param s 
    # ## #  Single character string. Name of the ternary classification to 
    # ## #  be fetched.
    # ## #
    # ## #@param ttt 
    # ## #  Value returned if s clock is TTT
    # ## #
    # ## #@param txf 
    # ## #  Value returned if s clock is TXF
    # ## #
    # ## #@param ftx 
    # ## #  Value returned if s clock is FTX
    # ## #
    # ## #@param fff 
    # ## #  Value returned if s clock is FFF
    # ## #
    # ## #
    # ## #@return 
    # ## #  A \code{\link[ternaryplot]{ternarySystem}} object.
    # ## #
    # ## #
    # ## #@rdname ternaryClockSwitch 
    # ## #
    # ## #@export 
    # ## #
    # ## #@keywords internal
    # ## #
    # .ternaryClockSwitch <- function( 
     # s, 
     # ttt, 
     # txf, 
     # ftx, 
     # fff 
    # ){  
        # if( is.character( s ) ){ 
            # s <- getTernarySystem( s )
        # }   
        
        # .blrClock <- blrClock( s )
        
        # if( identical( .blrClock, c( TRUE,  TRUE,  TRUE ) ) ){ 
            # out <- ttt 
        # }else if( identical( .blrClock, c( TRUE,  NA,    FALSE ) ) ){ 
            # out <- txf 
        # }else if( identical( .blrClock, c( FALSE, TRUE,  NA ) ) ){ 
            # out <- ftx 
        # }else if( identical( .blrClock, c( FALSE, FALSE, FALSE ) ) ){ 
            # out <- fff 
        # }else{ 
            # stop( "unknown value for blrClock( s ): %s", 
                # paste( .blrClock, collapse = ", " )  ) 
        # }   
        
        # return( out ) 
    # }   


    