
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryWindow =================================================

#'Set up World Coordinates for a ternary plot (invisible base plot)
#'
#'Set up World Coordinates for a ternary plot (invisible base plot).
#'
#'  Notice that \code{ternaryWindow} sets \code{par(pty='s')} 
#'  (i.e. for a square plot-region).
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, as created with 
#'  \code{\link[ternaryplot]{createTernarySystem}}, or a single 
#'  \code{character} string. Can be missing.
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a \code{\link[base]{matrix}} 
#'  containing point ternary data (x-y-x) to be ploted on the graph. 
#'  It should contain the 3 columns names given in \code{s}. Can 
#'  be missing or \code{NULL}.
#'
#'@param scale 
#'  A \code{\link[base]{data.frame}} with 3 columns and 2 rows. 
#'  Contains the min and max limits of each of the 3 variables 
#'  (columns = variables, rows = min and max).
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#' 
#'@rdname ternaryWindow-methods
#'
#'@export 
#'
ternaryWindow <- function( 
    s, 
    ... 
){  
    if( missing(s) ){ 
        UseMethod( "ternaryWindow", object = character(0) ) 
    }else{ 
        UseMethod( "ternaryWindow" ) 
    }   
}   



#'@rdname ternaryWindow-methods
#'
#'@method ternaryWindow character
#'
#'@export
#'
ternaryWindow.character <- function(
 s, 
 x, 
 scale = FALSE, 
 ... 
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    ternaryWindow( s = s, ... ) 
}   



#'@rdname ternaryWindow-methods
#'
#'@method ternaryWindow ternarySystem
#'
#'@export
#'
#'@importFrom graphics par
#'@importFrom graphics plot
ternaryWindow.ternarySystem <- function(
 s, 
 x, 
 scale = FALSE, 
 ... 
){  
    # Set x if it is missing
    if( missing( "x" ) ){ 
        x <- data.frame(
            "B"    = numeric(0), 
            "L"    = numeric(0), 
            "R"    = numeric(0)  
        )   
        
        colnames( x ) <- blrNames.ternarySystem( s = s ) 
        
    }else if( is.matrix( x ) ){ 
        x <- as.data.frame( x ) 
        
    }else if( !is.data.frame(x) ){ 
        stop( "'x' can be missing, a data.frame, or a matrix" ) 
    }   
    
    
    if( is.data.frame( scale ) ){ 
        # Test that the scale is correct
        ternaryCheck.ternarySystem( s = s, x = scale,  ) 
        
    }else if( is.logical( scale ) ){ 
        if( scale ){ 
            if( nrow(x) == 0 ){ 
                stop( "'scale' can not be 'TRUE' when 'x' is missing or with 0 rows" ) 
            }   
            
            # Find the optimal isocele triangle around the data
            scale <- .ternaryLims( x = x, s = s ) 
            
        }else{ 
            scale <- s[[ 'scale' ]] 
        }   
    }   
    
    
    ## Convert the scale into a triangular frame
    tpBox <- data.frame( 
        "B"    = c( scale[1,1], scale[2,1], scale[1,1] ), 
        "L"    = c( scale[1,2], scale[1,2], scale[2,2] ), 
        "R"    = c( scale[2,3], scale[1,3], scale[1,3] ), 
        "row.names" = c( "left", "right", "top" ) 
    )   
    
    colnames( tpBox ) <- blrNames.ternarySystem( s = s )   
    
    
    # Convert the scale to x-y values
    
    # Create a 90 degree triangle
    .blrClock <- blrClock.ternarySystem( s ) 
    
    s90 <- s 

    if( is.na( .blrClock[2] ) ){ 
        # `tlrAngles<-.ternarySystem`( s = s90, value = c( 45, 45, 90 ) ) 
        
        tlrAngles( s = s90 ) <- c( 45, 45, 90 )
    }else if( .blrClock[2] ){ 
        # `tlrAngles<-.ternarySystem`( s = s90, value = c( 45, 90, 45 ) ) 
        
        tlrAngles( s = s90 ) <- c( 45, 90, 45 )
    }else{ 
        # `tlrAngles<-.ternarySystem`( s = s90, value = c( 45, 45, 90 ) )
        
        tlrAngles( s = s90 ) <- c( 45, 45, 90 )
    }   
    
    tpBox <- ternary2xy.ternarySystem( s = s90, x = tpBox ) 
    
    # Draw the plot
    oldPar <- graphics::par( 
        # mar = c(5.1, 4.1, 4.1, 4.1), # Margins c(bottom, left, top, right)
        pty = "s",                     # Plot region is 'square' (equal ratio)
        xpd = TRUE                     # Plotting can also occur out of the plot
    )    
    
    graphics::plot( 
        x    = tpBox[,"x"], 
        y    = tpBox[,"y"], 
        bty  = "n", # no box around, 
        xaxt = "n", # no x axis 
        yaxt = "n", # no y axis 
        type = "n", # no data visible 
        xlab = "",  # no x label
        ylab = "",  # no y label
        xlim = range( tpBox[,"x"] ), # No extra space 
        ylim = range( tpBox[,"y"] )  # No extra space 
    )   
    
    # polygon( x = tpBox[, "x" ], y = tpBox[, "y" ] ) 
    
    s[[ 'scale' ]] <- scale 
    
    graphics::par( oldPar[ names( oldPar ) != "pty" ] ) 
    
    return( invisible( s ) ) 
}   


