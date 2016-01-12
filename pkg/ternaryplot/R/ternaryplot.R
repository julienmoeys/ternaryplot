
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
#'Set up World Coordinates for a ternary plot (invisible base plot)
#'
#'
#'@param s 
#'  A \code{ternarySystem} object, as created with 
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
        
        colnames( x ) <- blrNames( s = s ) 
        
    }else if( is.matrix( x ) ){ 
        x <- as.data.frame( x ) 
        
    }else if( !is.data.frame(x) ){ 
        stop( "'x' can be missing, a data.frame, or a matrix" ) 
    }   
    
    
    if( is.data.frame( scale ) ){ 
        # Test that the scale is correct
        scale <- ternaryData( s = s, x = scale,  ) 
        
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
    
    colnames( tpBox ) <- blrNames( s = s )   
    
    
    # Convert the scale to x-y values
    
    # Create a 90 degree triangle
    .blrClock <- blrClock( s ) 
    
    s90 <- s 

    if( is.na( .blrClock[2] ) ){ 
        tlrAngles( s90 ) <- c( 45, 45, 90 ) 
    }else if( .blrClock[2] ){ 
        tlrAngles( s90 ) <- c( 45, 90, 45 ) 
    }else{ 
        tlrAngles( s90 ) <- c( 45, 45, 90 ) 
    }   
    
    tpBox <- ternary2xy( x = tpBox, s = s90 ) 
    
    # Draw the plot
    par( 
        # mar = c(5.1, 4.1, 4.1, 4.1), # Margins c(bottom, left, top, right)
        pty = "s",                   # Plot region is 'square' (equal ratio)
        xpd = TRUE                   # Plotting can also occur out of the plot
    )    
    
    plot( 
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
    
    return( invisible( s ) ) 
}   



# ternaryBox ====================================================

#'Draw a Box around a ternary plot
#'
#'Draw a Box around a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{box}}.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{polygon}}.
#'  You can for instance set \code{border} for the color of the 
#'  box outline, \code{col}, for the fill-color of the box, 
#'  \code{lwd} (outline thickness) or \code{lty} (line-type).
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates or 
#'  a \code{Spatial*} objects (see \code{.plot}).
#'
#'
#'@rdname ternaryBox-methods
#'
#'@export 
#'
ternaryBox <- function( s, ... ){ 
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryBox)" ) 
    }   
    
    UseMethod( "ternaryBox" ) 
}   



# INTERNAL. Converts x-y points to SpatialPolygons.
#   The polygon is closed internally (first value added as last 
#   value)
#'@importFrom sp Polygon
#'@importFrom sp Polygons
#'@importFrom sp SpatialPolygons
.xy2SpatialPolygons <- function( xy ){ 
    if( !identical( xy[1], xy[ nrow( xy ), ] ) ){ 
        xy <- rbind( xy, xy[1,] ) 
    }   
    
    p <- sp::Polygon( coords = xy )
    p <- sp::Polygons( srl = list( p ), ID = 1L )
    p <- sp::SpatialPolygons( Srl = list( p ), pO = 1L ) 
    
    return( p ) 
}   



#'@rdname ternaryBox-methods
#'
#'@method ternaryBox ternarySystem
#'
#'@export
#' 
ternaryBox.ternarySystem <- function( 
 s, 
 .plot = TRUE, 
 ... 
){  
    axis.line.lwd <- getTpPar( "axis.line.lwd" )
    fg            <- par( "fg" )
    
    scale <- s[[ 'scale' ]]
    
    ## Convert the scale into a triangular frame
    tpBox <- data.frame( 
        "B"    = c( scale[ 1, 1 ], scale[ 2, 1 ], scale[ 1, 1 ] ), 
        "L"    = c( scale[ 1, 2 ], scale[ 1, 2 ], scale[ 2, 2 ] ), 
        "R"    = c( scale[ 2, 3 ], scale[ 1, 3 ], scale[ 1, 3 ] ), 
        "row.names" = c( "left", "right", "top" ) 
    )   
    
    colnames( tpBox ) <- blrNames( s = s )   
    
    
    # Convert the scale to x-y values
    tpBox <- ternary2xy( s = s, x = tpBox ) 
    
    if( .plot ){ 
        polygon( x = tpBox[, "x" ], y = tpBox[, "y" ], 
            lwd = axis.line.lwd, border = fg, ... )
    }   
    
    out <- tpBox[, c( "x", "y" ) ] 
    if( getTpPar( "sp" ) ){ out <- .xy2SpatialPolygons( xy = out ) }
    
    return( invisible( out ) ) 
}   



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
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[graphics]{segments}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates or 
#'  a \code{Spatial*} objects (see \code{.plot}).
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



# Converts x-y segments to sp::SpatialLines
#'@importFrom sp SpatialLines
#'@importFrom sp Lines
#'@importFrom sp Line
.xySegments2SpatialLines <- function( fromXY, toXY ){ 
    coords <- data.frame( 
        "x" = rep( NA_real_, 2 ), 
        "y" = rep( NA_real_, 2 ) ) 
    
    out <- sp::SpatialLines( lapply( 
        X   = 1:nrow(fromXY), 
        FUN = function(i){ 
            coords[, "x" ] <- c( fromXY[ i, "x" ], toXY[ i, "x" ] ) 
            coords[, "y" ] <- c( fromXY[ i, "y" ], toXY[ i, "y" ] ) 
            
            return( sp::Lines( list( sp::Line( coords = coords ) ), ID = i ) )
        }   
    ) ) 
    
    return( out )
}   



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
 .plot = TRUE, 
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
    .blrNames <- blrNames( s = s ) 
    from <- from[, .blrNames ]  
    to   <- to[, .blrNames ]   
    
    # Transform the coordinates into x-y values
    fromXY <- ternary2xy( x = from, s = s ) 
    toXY   <- ternary2xy( x = to, s = s ) 
    
    # Draw the segments
    if( .plot ){ 
        segments(
            x0  = fromXY[, "x" ], 
            y0  = fromXY[, "y" ], 
            x1  = toXY[, "x" ], 
            y1  = toXY[, "y" ], 
            ...
        )   
    }   
    
    out <- list( "from" = fromXY, "to" = toXY ) 
    
    if( getTpPar( "sp" ) ){ 
        out <- .xySegments2SpatialLines( fromXY = fromXY, toXY = toXY ) 
    }   
    
    return( invisible( out ) ) 
}   



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
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{arrows}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates or 
#'  a \code{Spatial*} objects (see \code{.plot}).
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
 .plot = TRUE, 
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
    .blrNames <- blrNames( s = s ) 
    from <- from[, .blrNames ]  
    to   <- to[, .blrNames ]   
    
    # Transform the coordinates into x-y values
    fromXY <- ternary2xy( x = from, s = s ) 
    toXY   <- ternary2xy( x = to, s = s ) 
    
    # Draw the arrows
    if( .plot ){ 
        arrows(
            x0  = fromXY[, "x" ], 
            y0  = fromXY[, "y" ], 
            x1  = toXY[, "x" ], 
            y1  = toXY[, "y" ], 
            ...
        )   
    }   
    
    out <- list( "from" = fromXY, "to" = toXY ) 
    
    if( getTpPar( "sp" ) ){ 
        out <- .xySegments2SpatialLines( fromXY = fromXY, toXY = toXY ) 
    }   
    
    return( invisible( out ) ) 
}   



# .ternaryGridBase ==============================================

#'INTERNAL. Calculates start- and end-points of grid segments (tick-marks, ...) in ternary coordinates
#'
#'INTERNAL. Calculates start- and end-points of grid segments (tick-marks, ...) in ternary coordinates
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param type
#'  Single character string. If \code{"grid"}, return values 
#'  the grid in a ternary plot, if \code{"ticks"} return values 
#'  for axis' tick-marks and if \code{"tickLabels"} returns 
#'  values for axis' tick-marks' labels.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#' 
#'@rdname ternaryGridBase-methods
#'
#'@export 
#'
#'@keywords internal
#'
.ternaryGridBase <- function( 
 s, 
 type = "grid", 
 ... 
){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like .ternaryGridBase)" ) 
    }   
    
    UseMethod( ".ternaryGridBase" ) 
}   


#'@rdname ternaryGridBase-methods
#'
#'@method .ternaryGridBase ternarySystem
#'
#'@export
#'
.ternaryGridBase.ternarySystem <- function( 
 s, 
 type = "grid", 
 ... 
){  
    .blrNames    <- blrNames( s = s )  
    .blrClock    <- blrClock( s )  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum( s = s ) 
    marginSize   <- .nbMargin2diffXY()
    .par         <- par() 
    
    if( type == "ticks" ){ 
        # ticksShiftTo <- getTpPar( "ticksShift" ) 
        ticksShift <- getTpPar( "ticksShift" ) 
        
        if( is.na( ticksShift ) ){
            ticksShift <- (marginSize / .fracSum) * (-1 * .par[[ "tcl" ]])
        }   
        
        ticksShiftFrom <- (marginSize / .fracSum) * .par[[ "mgp" ]][ 3L ] 
        ticksShiftTo   <- ticksShiftFrom + ticksShift
        
    }else if( type == "tickLabels" ){
        ticksLabelsShift <- getTpPar( "ticksLabelsShift" ) 
        
        if( is.na( ticksLabelsShift ) ){
            ticksLabelsShift <- abs( diff( .par[[ "mgp" ]][ 3:2 ] ) )
        }   
        
        ticksShiftFrom <- (marginSize / .fracSum) * .par[[ "mgp" ]][ 3L ] 
        ticksShiftTo   <- ticksShiftFrom + (marginSize / .fracSum) * ticksLabelsShift 
        
    }else{
        ticksShiftFrom <- NA_real_
        ticksShiftTo   <- NA_real_
    }   
    
    # Note: Axis order / index is Bottom -> Left -> Right
    #       This order is cyclic: after Right comes left
    #                             before Bottom, comes Right    
    
    
    # Fetch the ticks location:
    bTicks <- lTicks <- rTicks <- getTpPar( "ticksAt" ) * .fracSum 
    
    
    # #   For the case where axis orientation is NA
    # at1s  <- bTicks,         # Start values of lines on each side of the triangle
    # at2s  <- 1 - bTicks,     # (Start values) For grid lines: reverse value, for ticks, at + ticks.shift
    # at3s  <- 0,              # (Start values) For grid lines: 0 value, for ticks, 0 - ticks.shift
    # at1e  <- bTicks,         # End values of lines on each side of the triangle
    # at2e  <- 0,              # at.2.e: (End values) logically equal to at.3.s
    # at3e  <- 1 - bTicks,     # at.3.e: (End values) logically equal to at.2.s    
    
    
    # Select only the ticks that fit within the scale
    bTicks <- bTicks[ 
        bTicks > tScale[ "min", 1 ] & 
        bTicks < tScale[ "max", 1 ]   
    ]   
    
    lTicks <- lTicks[ 
        lTicks > tScale[ "min", 2 ] & 
        lTicks < tScale[ "max", 2 ]   
    ]   
    
    rTicks <- rTicks[ 
        rTicks > tScale[ "min", 3 ] & 
        rTicks < tScale[ "max", 3 ]   
    ]   
    
    
    
    # pre-format the grid-lines of each axis
    bTicks <- data.frame( 
        "B" = bTicks, 
        "L" = rep( NA_real_, length(bTicks) ), 
        "R" = rep( NA_real_, length(bTicks) )
    )   
    colnames( bTicks ) <- .blrNames 
    
    lTicks <- data.frame( 
        "B" = rep( NA_real_, length(lTicks) ), 
        "L" = lTicks, 
        "R" = rep( NA_real_, length(lTicks) )
    )   
    colnames( lTicks ) <- .blrNames 
    
    rTicks <- data.frame( 
        "B" = rep( NA_real_, length(rTicks) ), 
        "L" = rep( NA_real_, length(rTicks) ), 
        "R" = rTicks
    )   
    colnames( rTicks ) <- .blrNames 
    
    # Format as a list
    gridFromTo <- list( 
        "B" = bTicks, 
        "L" = lTicks, 
        "R" = rTicks  
    )   
    names( gridFromTo ) <- .blrNames
    
    rm( bTicks, lTicks, rTicks )
    
    
    #   Call geometry specific methods
    out <- ._ternaryGridBase(
        s               = s, 
        type            = type, 
        ticksShiftFrom  = ticksShiftFrom, 
        ticksShiftTo    = ticksShiftTo, 
        gridFromTo      = gridFromTo, 
        ... 
    )   
    
    return( out ) 
}   


## # INTERNAL. Geometry specific methods for .ternaryGridBase()
## #
## # INTERNAL. Geometry specific methods for .ternaryGridBase()
## #
## #
## #@param s 
## #  See .ternaryGridBase()
## #
## #@param type
## #  See .ternaryGridBase()
## #
## #@param \dots
## #  See .ternaryGridBase()
## #
## # 
## #@rdname _ternaryGridBase-methods
## #
## #@export 
## #
## #@keywords internal
## #
._ternaryGridBase <- function( 
 s, 
 type, 
 ticksShiftFrom, 
 ticksShiftTo, 
 gridFromTo, 
 ... 
){  
    UseMethod( "._ternaryGridBase" ) 
}   


## #@rdname _ternaryGridBase-methods-methods
## #
## #@method ._ternaryGridBase geo_TTT
## #
## #@export
## #
._ternaryGridBase.geo_TTT <- function( 
 s, 
 type, 
 ticksShiftFrom, 
 ticksShiftTo, 
 gridFromTo, 
 ... 
){  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum( s = s ) 
    
    gridFrom <- gridTo <- gridFromTo; rm( gridFromTo )
    
    
    ## Calculate the grid-lines coordinates for each axis
    for( ax in 1:3 ){ 
        # Index of previous and next axis 
        if( ax == 1 ){ 
            axPrev <- 3 
            axNext <- 2 
        }else if( ax == 2 ){ 
            axPrev <- 1 
            axNext <- 3 
        }else if( ax == 3 ){ 
            axPrev <- 2 
            axNext <- 1 
        }   
        
        if( type == "grid" ){ # !ticks
            # Start coordinates on next axis is 0 or min
            gridFrom[[ ax ]][, axNext ] <- tScale[ "min", axNext ] 
            
            # Start coordinates on previous axis
            gridFrom[[ ax ]][, axPrev ] <- 
                .fracSum - 
                gridFrom[[ ax ]][, ax ] - 
                gridFrom[[ ax ]][, axNext ]
            
            # End coordinates on previous axis
            gridTo[[ ax ]][, axPrev ] <- tScale[ "min", axPrev ] 
        
            # End coordinate on next axis
            gridTo[[ ax ]][, axNext ] <- 
                .fracSum - 
                gridTo[[ ax ]][, ax ] - 
                gridTo[[ ax ]][, axPrev ]
        }else{ 
            # Ticks or ticks labels
            
            # Start coordinates on next axis is 0 or min
            gridFrom[[ ax ]][, axNext ] <- tScale[ "min", axNext ] - ticksShiftFrom * .fracSum  
            
            # Start coordinates on previous axis
            gridFrom[[ ax ]][, axPrev ] <- 
                .fracSum - 
                gridFrom[[ ax ]][, ax ] - 
                gridFrom[[ ax ]][, axNext ]
            
            # End coordinates on previous axis
            gridTo[[ ax ]][, axNext ] <- tScale[ "min", axNext ] - ticksShiftTo * .fracSum  
        
            # End coordinate on next axis
            gridTo[[ ax ]][, axPrev ] <- 
                .fracSum - 
                gridTo[[ ax ]][, ax ] - 
                gridTo[[ ax ]][, axNext ]
        }   
    }   
    
    # Format the output
    out <- list( 
        "from" = gridFrom, 
        "to"   = gridTo 
    )   
    
    return( out ) 
}   


## #@rdname _ternaryGridBase-methods-methods
## #
## #@method ._ternaryGridBase geo_FFF
## #
## #@export
## #
._ternaryGridBase.geo_FFF <- function( 
 s, 
 type, 
 ticksShiftFrom, 
 ticksShiftTo, 
 gridFromTo, 
 ... 
){  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum( s = s ) 
    
    gridFrom <- gridTo <- gridFromTo; rm( gridFromTo )
    
    
    ## Calculate the grid-lines coordinates for each axis
    for( ax in 1:3 ){ 
        # Index of previous and next axis 
        if( ax == 1 ){ 
            axPrev <- 3 
            axNext <- 2 
            
        }else if( ax == 2 ){ 
            axPrev <- 1 
            axNext <- 3 
            
        }else if( ax == 3 ){ 
            axPrev <- 2 
            axNext <- 1 
            
        }   
        
        if( type == "grid" ){ # !ticks
            # Start coordinates on previous axis is 0 or min
            gridFrom[[ ax ]][, axPrev ] <- tScale[ "min", axPrev ] 
            
            # Start coordinates on next axis
            gridFrom[[ ax ]][, axNext ] <- 
                .fracSum - 
                gridFrom[[ ax ]][, ax ] - 
                gridFrom[[ ax ]][, axPrev ]
            
            # End coordinates on next axis
            gridTo[[ ax ]][, axNext ] <- tScale[ "min", axNext ] 
            
            # End coordinate on previous axis
            gridTo[[ ax ]][, axPrev] <- 
                .fracSum - 
                gridTo[[ ax ]][, ax ] - 
                gridTo[[ ax ]][, axNext ]
        }else{ 
            #   Ticks or ticks labels
            
            # Start coordinates on previous axis is 0 or min
            gridFrom[[ ax ]][, axPrev ] <- tScale[ "min", axPrev ] - ticksShiftFrom * .fracSum 
            
            # Start coordinates on next axis
            gridFrom[[ ax ]][, axNext ] <- 
                .fracSum - 
                gridFrom[[ ax ]][, ax ] - 
                gridFrom[[ ax ]][, axPrev ]
            
            # End coordinates on next axis
            gridTo[[ ax ]][, axPrev ] <- tScale[ "min", axPrev ] - ticksShiftTo * .fracSum 
            
            # End coordinate on previous axis
            gridTo[[ ax ]][, axNext ] <- 
                .fracSum - 
                gridTo[[ ax ]][, ax ] - 
                gridTo[[ ax ]][, axPrev ] 
        }   
    }   
    
    # Format the output
    out <- list( 
        "from" = gridFrom, 
        "to"   = gridTo 
    )   
    
    return( out ) 
}   


## #@rdname _ternaryGridBase-methods-methods
## #
## #@method ._ternaryGridBase geo_FTX
## #
## #@export
## #
._ternaryGridBase.geo_FTX <- function( 
 s, 
 type, 
 ticksShiftFrom, 
 ticksShiftTo, 
 gridFromTo, 
 ... 
){  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum( s = s ) 
    
    gridFrom <- gridTo <- gridFromTo; rm( gridFromTo )
    
    # Calculate the grid-lines coordinates for each axis
    # ======================================================
    
    # Axis 1: bottom (counter-clockwise)
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks
        # Start coordinates on next axis is 0 or min
        gridFrom[[ 1L ]][, 2L ] <- tScale[ "min", 2L ] 
        
        # Start coordinates on previous axis
        gridFrom[[ 1L ]][, 3L ] <- 
            .fracSum - 
            gridFrom[[ 1L ]][, 1L ] - 
            gridFrom[[ 1L ]][, 2L ]
        
        # End coordinates on previous axis
        gridTo[[ 1L ]][, 3L ] <- tScale[ "min", 3L ] 
        
        # End coordinate on next axis
        gridTo[[ 1L ]][, 2L ] <- 
            .fracSum - 
            gridTo[[ 1L ]][, 1L ] - 
            gridTo[[ 1L ]][, 3L ]
    }else{ 
        #   Ticks or tick labels
        
        # Start coordinates on next axis is 0 or min
        gridFrom[[ 1L ]][, 2L ] <- tScale[ "min", 2L ] - ticksShiftFrom * .fracSum  
        
        # Start coordinates on previous axis
        gridFrom[[ 1L ]][, 3L ] <- 
            .fracSum - 
            gridFrom[[ 1L ]][, 1L ] - 
            gridFrom[[ 1L ]][, 2L ]
        
        # End coordinates on previous axis
        gridTo[[ 1L ]][, 2L ] <- tScale[ "min", 2L ] - ticksShiftTo * .fracSum  
        
        # End coordinate on next axis
        gridTo[[ 1L ]][, 3L ] <- 
            .fracSum - 
            gridTo[[ 1L ]][, 1L ] - 
            gridTo[[ 1L ]][, 2L ]
    }   
    
    # Axis 2: left (clockwise)
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks 
        # Start coordinates on previous axis is 0 or min
        gridFrom[[ 2L ]][, 1L ] <- tScale[ "min", 1L ] 
        
        # Start coordinates on next axis
        gridFrom[[ 2L ]][, 3L ] <- 
            .fracSum - 
            gridFrom[[ 2L ]][, 2L ] - 
            gridFrom[[ 2L ]][, 1L ]
        
        # End coordinates on next axis
        gridTo[[ 2L ]][, 3L ] <- tScale[ "min", 3L ] 
    
        # End coordinate on previous axis
        gridTo[[ 2L ]][, 1L ] <- 
            .fracSum - 
            gridTo[[ 2L ]][, 2L ] - 
            gridTo[[ 2L ]][, 3L ] 
    }else{ 
        # Start coordinates on previous axis is 0 or min
        gridFrom[[ 2L ]][, 1L ] <- tScale[ "min", 1L ] - ticksShiftFrom * .fracSum 
        
        # Start coordinates on next axis
        gridFrom[[ 2L ]][, 3L ] <- 
            .fracSum - 
            gridFrom[[ 2L ]][, 2L ] - 
            gridFrom[[ 2L ]][, 1L ]
        
        # End coordinate on previous axis
        gridTo[[ 2L ]][, 1L ] <- tScale[ "min", 1L ] - ticksShiftTo * .fracSum 
        
        # End coordinate on next axis
        gridTo[[ 2L ]][, 3L ] <- 
            .fracSum - 
            gridTo[[ 2L ]][, 2L ] - 
            gridTo[[ 2L ]][, 1L ] 
    }   
    
    # Axis 3: NA 
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks
        gridFrom[[ 3L ]][, 1L ] <- gridFrom[[ 1L ]][, 1L ]              # bottom
        gridFrom[[ 3L ]][, 2L ] <- 0                                    # left
        gridFrom[[ 3L ]][, 3L ] <- .fracSum - gridFrom[[ 1L ]][, 1L ]   # right
        
        # data.frame( 
            # "B" = bTicks[ .blrNames[ 1 ] ], 
            # "L" = 0,
            # "R" = .fracSum - bTicks[ .blrNames[ 1 ] ] 
        # )   
        # colnames( gridFrom[[ 3L ]] ) <- .blrNames 
        
        gridTo[[ 3L ]][, 1L ] <- 0                                  # bottom
        gridTo[[ 3L ]][, 2L ] <- gridFrom[[ 1L ]][, 1L ]            # left
        gridTo[[ 3L ]][, 3L ] <- .fracSum - gridFrom[[ 1L ]][, 1L ] # right
        
        # data.frame( 
            # "B" = 0,  
            # "L" = bTicks[ .blrNames[ 1 ] ],
            # "R" = .fracSum - bTicks[ .blrNames[ 1 ] ] 
        # )   
        # colnames( gridTo[[ 3L ]] )   <- .blrNames 
        
    }else{ 
        #   Ticks and ticks labels
        
        gridFrom[[ 3L ]] <- data.frame() 
        gridTo[[ 3L ]]   <- data.frame() 
    }   
    
    # Format the output
    out <- list( 
        "from" = gridFrom, 
        "to"   = gridTo 
    )   
    
    return( out ) 
}   


## #@rdname _ternaryGridBase-methods-methods
## #
## #@method ._ternaryGridBase geo_TXF
## #
## #@export
## #
._ternaryGridBase.geo_TXF <- function( 
 s, 
 type, 
 ticksShiftFrom, 
 ticksShiftTo, 
 gridFromTo, 
 ... 
){  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum( s = s ) 
    
    gridFrom <- gridTo <- gridFromTo; rm( gridFromTo )
    
    # Calculate the grid-lines coordinates for each axis
    # ======================================================
    
    # Axis 1: bottom (clockwise)
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks 
        # Start coordinates on previous axis is 0 or min
        gridFrom[[ 1L ]][, 3L ] <- tScale[ "min", 3L ] 
        
        # Start coordinates on next axis
        gridFrom[[ 1L ]][, 2L ] <- 
            .fracSum - 
            gridFrom[[ 1L ]][, 1L ] - 
            gridFrom[[ 1L ]][, 3L ]
        
        # End coordinates on next axis
        gridTo[[ 1L ]][, 2L ] <- tScale[ "min", 2L ] 
    
        # End coordinate on previous axis
        gridTo[[ 1L ]][, 3L ] <- 
            .fracSum - 
            gridTo[[ 1L ]][, 1L ] - 
            gridTo[[ 1L ]][, 2L ] 
    }else{ 
        # Start coordinates on previous axis is 0 or min
        gridFrom[[ 1L ]][, 3L ] <- tScale[ "min", 3L ] - ticksShiftFrom * .fracSum 
        
        # Start coordinates on next axis
        gridFrom[[ 1L ]][, 2L ] <- 
            .fracSum - 
            gridFrom[[ 1L ]][, 1L ] - 
            gridFrom[[ 1L ]][, 3L ]
        
        # End coordinate on previous axis
        gridTo[[ 1L ]][, 3L ] <- tScale[ "min", 3L ] - ticksShiftTo * .fracSum 
        
        # End coordinate on next axis
        gridTo[[ 1L ]][, 2L ] <- 
            .fracSum - 
            gridTo[[ 1L ]][, 1L ] - 
            gridTo[[ 1L ]][, 3L ] 
    }   
    
    # Axis 2: left (NA)
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks
        gridFrom[[ 2L ]][, 1L ] <- gridFrom[[ 1L ]][, 1L ] 
        gridFrom[[ 2L ]][, 2L ] <- .fracSum - gridFrom[[ 1L ]][, 1L ] 
        gridFrom[[ 2L ]][, 3L ] <- 0 
        
        # data.frame( 
            # "B" = bTicks[ .blrNames[ 1 ] ], 
            # "L" = .fracSum - bTicks[ .blrNames[ 1 ] ],  
            # "R" = 0 
        # )   
        # colnames( gridFrom[[ 2L ]] ) <- .blrNames 
        
        gridTo[[ 2L ]][, 1L ] <- 0 
        gridTo[[ 2L ]][, 2L ] <- .fracSum - gridFrom[[ 1L ]][, 1L ]
        gridTo[[ 2L ]][, 3L ] <- gridFrom[[ 1L ]][, 1L ]
        
        # data.frame( 
            # "B" = 0,  
            # "L" = .fracSum - bTicks[ .blrNames[ 1 ] ], 
            # "R" = bTicks[ .blrNames[ 1 ] ] 
        # )   
        # colnames( gridTo[[ 2L ]] )   <- .blrNames 
        
    }else{ 
        gridFrom[[ 2L ]] <- data.frame() 
        gridTo[[ 2L ]]   <- data.frame() 
    }   
    
    # Axis 3: right (counter-clockwise) 
    # ------------------------------------------------------
    
    if( type == "grid" ){ # !ticks
        # Start coordinates on next axis is 0 or min
        gridFrom[[ 3L ]][, 1L ] <- tScale[ "min", 1L ] 
        
        # Start coordinates on previous axis
        gridFrom[[ 3L ]][, 2L ] <- 
            .fracSum - 
            gridFrom[[ 3L ]][, 3L ] - 
            gridFrom[[ 3L ]][, 1L ]
        
        # End coordinates on previous axis
        gridTo[[ 3L ]][, 2L ] <- tScale[ "min", 2L ] 
        
        # End coordinate on next axis
        gridTo[[ 3L ]][, 1L ] <- 
            .fracSum - 
            gridTo[[ 3L ]][, 3L ] - 
            gridTo[[ 3L ]][, 2L ]
    }else{ 
        # Start coordinates on next axis is 0 or min
        gridFrom[[ 3L ]][, 1L ] <- tScale[ "min", 1L ] - ticksShiftFrom * .fracSum  
        
        # Start coordinates on previous axis
        gridFrom[[ 3L ]][, 2L ] <- 
            .fracSum - 
            gridFrom[[ 3L ]][, 3L ] - 
            gridFrom[[ 3L ]][, 1L ]
        
        # End coordinates on previous axis
        gridTo[[ 3L ]][, 1L ] <- tScale[ "min", 1L ] - ticksShiftTo * .fracSum  
        
        # End coordinate on next axis
        gridTo[[ 3L ]][, 2L ] <- 
            .fracSum - 
            gridTo[[ 3L ]][, 3L ] - 
            gridTo[[ 3L ]][, 1L ]
    }   
    
    
    # Format the output
    out <- list( 
        "from" = gridFrom, 
        "to"   = gridTo 
    )   
    
    return( out ) 
    
}   




# .ternaryTicks =================================================

#'INTERNAL: Draw axis' tick marks on a triangle plot
#'
#'INTERNAL: Draw axis' tick marks on a triangle plot
#'
#'
#'@details 
#'  Ticks colors can be changed via 
#'  \code{\link[graphics]{par}("fg")}
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param side
#'  A vector of integer specifying which side of the plot the axis is to
#'  be drawn on.  The axis is placed as follows: 1=below, 2=left, 3=right (note 
#'  that this differ from \code{\link[graphics]{axis}} where 3=above and 
#'  4=right). Default is to draw axis on the 3 sides.
#'
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternarySegments}}, except \code{col} 
#'  (see details)
#'
#' 
#'@return
#'  Invisibly returns a list of \code{data.frame} 
#'  with the start and end points of the grid segments 
#'  for each of the 3 axis.
#'
#' 
#'@rdname ternaryTicks-methods
#'
#'@export 
#'
#'@keywords internal
#'
.ternaryTicks <- function( 
 s, 
 ... 
){  
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like .ternaryTicks)" ) 
    }   
    
    UseMethod( ".ternaryTicks" ) 
}   


#'@rdname ternaryTicks-methods
#'
#'@method .ternaryTicks ternarySystem
#'
#'@export
#'
#'@importFrom sp rbind.SpatialLines
.ternaryTicks.ternarySystem <- function( 
 s, 
 side = 1:3, 
 .plot = TRUE, 
 ... 
){  
    # Calculates the tick-marks position
    grTm <- .ternaryGridBase( s = s, type = "ticks" ) 
    
    # Calculates the tick-marks' labels position
    grTl <- .ternaryGridBase( s = s, type = "tickLabels" ) 
    
    n <- length( grTm[[ "from" ]] )
    
    if( !all(side %in% 1:3) ){
        stop( sprintf( 
            "'side' must be a vector of 3 integers (1, 2 or/and 3). Now %s", 
            paste( side, collapse = ", " ) 
        ) ) 
    }   
    
    if( !(n %in% side) ){
        stop( "Internal error in .ternaryTicks.ternarySystem. n and side are inconsistent" )
    }   
    
    #   Prepare output
    out <- outLine <- vector( length = n, mode = "list" ) 
    names( out ) <- c( "B", "L", "R" ) 
    
    
    fg            <- par( "fg" ) 
    col.lab       <- par( "col.lab" ) 
    axis.line.lwd <- getTpPar( "axis.line.lwd" )
    
    
    #   Geometry specific adjustments
    adj <- ._ternaryTicks( s = s )
    
    
    for( ax in side ){ # 
        # Draw the tick-marks start and segments
        if( nrow( grTm[[ "from" ]][[ ax ]] ) != 0 ){ 
            # ternaryPoints( 
                # x   = grTm[[ "from" ]][[ ax ]], 
                # s   = s )  
            
            # Prevent tests
            if( .plot ){ 
                oldPar <- tpPar( par = "testRange" )        
                tpPar  <- tpPar( testRange = FALSE ) 
                
                # Draw the ticks segments
                out[[ ax ]] <- ternarySegments( 
                    from  = grTm[[ "from" ]][[ ax ]], 
                    to    = grTm[[ "to" ]][[ ax ]], 
                    s     = s, 
                    col   = fg, 
                    lwd   = axis.line.lwd, 
                    .plot = .plot, 
                    ... )  
                
                # Draw the axis line
                outLine[[ ax ]] <- ternarySegments( 
                    from  = grTm[[ "from" ]][[ ax ]][ 1L, , drop = FALSE ], 
                    to    = grTm[[ "from" ]][[ ax ]][ nrow( grTm[[ "from" ]][[ ax ]] ), , drop = FALSE ], 
                    s     = s, 
                    col   = fg, 
                    lwd   = axis.line.lwd, 
                    .plot = .plot, 
                    ... )  
                
                ternaryText( 
                    x      = grTl[[ "to" ]][[ ax ]], 
                    labels = as.character( grTl[[ "to" ]][[ ax ]][, ax ] ), 
                    s      = s, 
                    # pos  = 2, 
                    adj    = c( adj[[ "adj1" ]][ ax ], adj[[ "adj2" ]][ ax ] ), 
                    srt    = adj[[ "blrLabelAngles" ]][ ax ], 
                    col    = col.lab, 
                    # offset = -5, 
                    ... ) 
                
                # Set test again
                tpPar( par = oldPar ) 
            }   
        }   
    }   
    
    
    if( getTpPar( "sp" ) ){ 
        isNull <- unlist( lapply( X = out, FUN = is.null ) )
        
        out <- do.call( what = "rbind.SpatialLines", 
            args = c( out[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        isNull <- unlist( lapply( X = outLine, FUN = is.null ) )
        
        outLine <- do.call( what = "rbind.SpatialLines", 
            args = c( outLine[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        out <- sp::rbind.SpatialLines( out, outLine, makeUniqueIDs = TRUE )
        
        # spChFIDs( out ) <- c( "B", "L", "R" )[ !isNull ]
    }   
    
    return( invisible( out ) ) 
}   


## #INTERNAL: Geometry specific methods for .ternaryTicks()
## #
## #INTERNAL: Geometry specific methods for .ternaryTicks()
## #
## #
## #@param s 
## #  See .ternaryTicks()
## #
## #@param side
## #  See .ternaryTicks()
## #
## #@param .plot 
## #  See .ternaryTicks()
## #
## #@param \dots
## #  See .ternaryTicks()
## #
## # 
## #@return
## #  See .ternaryTicks()
## #
## # 
## #@rdname _ternaryTicks-methods
## #
## #@export 
## #
## #@keywords internal
## #
._ternaryTicks <- function( 
 s, 
 ... 
){  
    UseMethod( "._ternaryTicks" ) 
}   


## #@rdname _ternaryTicks-methods
## #
## #@method ._ternaryTicks geo_TTT
## #
## #@export
## #
._ternaryTicks.geo_TTT <- function( 
 s
){  
    .tlrAngle       <- tlrAngles( s = s ) 
    
    out <- list(
        "adj1"           = c( -0.2, +1.2, -0.2 ), 
        "adj2"           = c( +0.5, +0.5, +0.5 ), 
        "blrLabelAngles" = c( -.tlrAngle[3], 0, +.tlrAngle[2] ) 
    )   
    
    return( invisible( out ) ) 
}   


## #@rdname _ternaryTicks-methods
## #
## #@method ._ternaryTicks geo_FFF
## #
## #@export
## #
._ternaryTicks.geo_FFF <- function( 
 s
){  
    .tlrAngle       <- tlrAngles( s = s ) 
    
    out <- list(
        "adj1"           = c( +1.2, +1.2, -0.2 ), 
        "adj2"           = c( +0.5, +0.5, +0.5 ), 
        "blrLabelAngles" = c(  +.tlrAngle[2], -.tlrAngle[3], 0 ) 
    )   
    
    return( out ) 
}   


## #@rdname _ternaryTicks-methods
## #
## #@method ._ternaryTicks geo_FTX
## #
## #@export
## #
._ternaryTicks.geo_FTX <- function( 
 s
){  
    .tlrAngle       <- tlrAngles( s = s ) 
    
    out <- list(
        "adj1"           = c( +0.5, +1.2, -0.2 ), 
        "adj2"           = c( +1.2, +0.5, +0.5 ), 
        "blrLabelAngles" = c( 0, 0, NA ) 
    )   
    
    return( out ) 
}   


## #@rdname _ternaryTicks-methods
## #
## #@method ._ternaryTicks geo_TXF
## #
## #@export
## #
._ternaryTicks.geo_TXF <- function( 
 s
){  
    .tlrAngle       <- tlrAngles( s = s ) 
    
    out <- list(
        "adj1"           = c( +0.5, +1.2, -0.2 ), 
        "adj2"           = c( +1.2, +0.5,  +0.5 ), 
        "blrLabelAngles" = c( 0, NA, 0 ) 
    )   
    
    return( out ) 
}   




# ternaryGrid ===================================================

#'Add Grid to a ternary plot
#'
#'Add Grid to a ternary plot
#'
#'
#'@details 
#'  Grid-line colors can be changed via 
#'  \code{\link[ternaryplot]{tpPar}("grid.line.col")}.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param side
#'  A vector of integer specifying which side of the plot the axis is to
#'  be drawn on.  The axis is placed as follows: 1=below, 2=left, 3=right (note 
#'  that this differ from \code{\link[graphics]{axis}} where 3=above and 
#'  4=right). Default is to draw axis on the 3 sides.
#'
#'@param .plot 
#'  Single logical value. Set to \code{FALSE} if you don't want 
#'  to plot the graphical element and simply returns them as 
#'  x-y coordinates (or \code{Spatial*} objects if \code{sp} is 
#'  set to \code{TRUE} in \code{\link{tpPar}}).
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternarySegments}}, except \code{col} 
#'  (see details)
#'
#' 
#'@return
#'  Invisibly returns a list of \code{data.frame} 
#'  with the start and end points of the grid segments 
#'  for each of the 3 axis.
#'
#' 
#'@rdname ternaryGrid-methods
#'
#'@export 
#'
ternaryGrid <- function( s, ... ){ 
    if( missing( s ) ){ 
        stop( "'s' is missing. Required for low-level plotting commands (like ternaryGrid)" ) 
    }   
    
    UseMethod( "ternaryGrid" ) 
}   



#'@rdname ternaryGrid-methods
#'
#'@method ternaryGrid ternarySystem
#'
#'@export
#'
#'@importFrom sp rbind.SpatialLines
ternaryGrid.ternarySystem <- function( 
 s, 
 side = 1:3, 
 .plot = TRUE, 
 ... 
){  
    # Calculates the tick-marks and grid-segments position
    gr <- .ternaryGridBase( s = s, type = "grid" ) 
    
    grid.line.col <- getTpPar( "grid.line.col" )
    
    n <- length( gr[[ "from" ]] ) 
    
    if( !all(side %in% 1:3) ){
        stop( sprintf( 
            "'side' must be a vector of 3 integers (1, 2 or/and 3). Now %s", 
            paste( side, collapse = ", " ) 
        ) ) 
    }   
    
    if( !(n %in% side) ){
        stop( "Internal error in ternaryGrid.ternarySystem. n and side are inconsistent" )
    }   
    
    
    #   Prepare output
    out <- vector( length = n, mode = "list" ) 
    names( out ) <- c( "B", "L", "R" ) 
    
    
    for( ax in side ){ # 1:n
        if( nrow( gr[[ "from" ]][[ ax ]] ) != 0 ){ 
            # Draw the grid segments
            
            if( .plot ){ 
                out[[ ax ]] <- ternarySegments( 
                    from  = gr[[ "from" ]][[ ax ]], 
                    to    = gr[[ "to" ]][[ ax ]], 
                    s     = s, 
                    col   = grid.line.col, 
                    .plot = .plot, 
                    ... ) 
            }   
        }   
    }   
    
    
    if( getTpPar( "sp" ) ){ 
        isNull <- unlist( lapply( X = out, FUN = is.null ) )
        
        out <- do.call( what = "rbind.SpatialLines", 
            args = c( out[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        # spChFIDs( out ) <- c( "B", "L", "R" )[ !isNull ]
    }   
    
    return( invisible( out ) ) 
}   



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




# .ternaryAxisArrowsBase ========================================

#'INTERNAL. Calculates arrows segments start and end for ternary 
#'  axis labels 
#'
#'INTERNAL. Calculates arrows segments start and end for ternary 
#'  axis labels 
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#' 
#'@rdname ternaryAxisArrowsBase-methods
#'
#'@export 
#'
#'@keywords internal
#'
.ternaryAxisArrowsBase <- function( s, ... ){  
    UseMethod( ".ternaryAxisArrowsBase" ) 
}   


#'@rdname ternaryAxisArrowsBase-methods
#'
#'@method .ternaryAxisArrowsBase ternarySystem
#'
#'@export
#'
.ternaryAxisArrowsBase.ternarySystem <- function( 
 s, 
 ... 
){   
    .blrNames     <- blrNames( s = s )  
    .blrClock     <- blrClock( s )  
    tScale        <- s[[ 'scale' ]] 
    .fracSum      <- fracSum( s = s ) 
    arrowsShift   <- getTpPar( "arrowsShift" ) 
    arrowsCoords  <- getTpPar( "arrowsCoords" ) 
    
    if( any( is.na( arrowsShift ) ) ){
        #   Note: also set in .ternaryAxisArrows
        arrowsHeight <- getTpPar( "arrowsHeight" ) 
        mgp          <- par( "mgp" )
        
        arrowsShift <- (.nbMargin2diffXY() / .fracSum) * 
            c( mgp[ 1L ] - arrowsHeight, mgp[ 1L ] ) 
        
    }   
    
    
    #   Note: Axis order / index is Bottom -> Left -> Right
    #   This order is cyclic: after Right comes left
    #   before Bottom, comes Right    
    
    
    #   Min and max extent of each axis
    bAxisRange <- c( tScale[ "min", 1 ], tScale[ "max", 1 ] ) 
    bAxisRange[3] <- diff( bAxisRange ) # Width
    
    lAxisRange <- c( tScale[ "min", 2 ], tScale[ "max", 2 ] ) 
    lAxisRange[3] <- diff( lAxisRange ) # Width
    
    rAxisRange <- c( tScale[ "min", 3 ], tScale[ "max", 3 ] ) 
    rAxisRange[3] <- diff( rAxisRange ) # Width
    
    
    #   Pre-format a quadrilateral polygon on which the 
    #   axis arrows and arrow labels will be drawn
    #   Note: the arrow will be drawn on vertices 1 -> 2 -> 
    #   3. Point 4 is for anchoring the axis label
    bArrows <- data.frame( 
        "B" = c( 
            bAxisRange[1] + bAxisRange[3] * arrowsCoords[ 1 ], 
            bAxisRange[1] + bAxisRange[3] * arrowsCoords[ 2 ], 
            bAxisRange[1] + bAxisRange[3] * arrowsCoords[ 3 ], 
            bAxisRange[1] + bAxisRange[3] * arrowsCoords[ 4 ] 
        ),  
        "L" = rep( NA_real_, 4 ), 
        "R" = rep( NA_real_, 4 )
    )   
    colnames( bArrows ) <- .blrNames 
    
    lArrows <- data.frame( 
        "B" = rep( NA_real_, 4 ), 
        "L" = c( 
            lAxisRange[1] + lAxisRange[3] * arrowsCoords[ 1 ], 
            lAxisRange[1] + lAxisRange[3] * arrowsCoords[ 2 ], 
            lAxisRange[1] + lAxisRange[3] * arrowsCoords[ 3 ], 
            lAxisRange[1] + lAxisRange[3] * arrowsCoords[ 4 ] 
        ),  
        "R" = rep( NA_real_, 4 )
    )   
    colnames( lArrows ) <- .blrNames 
    
    rArrows <- data.frame( 
        "B" = rep( NA_real_, 4 ), 
        "L" = rep( NA_real_, 4 ), 
        "R" = c( 
            rAxisRange[1] + rAxisRange[3] * arrowsCoords[ 1 ], 
            rAxisRange[1] + rAxisRange[3] * arrowsCoords[ 2 ], 
            rAxisRange[1] + rAxisRange[3] * arrowsCoords[ 3 ], 
            rAxisRange[1] + rAxisRange[3] * arrowsCoords[ 4 ] 
        )   
    )   
    colnames( rArrows ) <- .blrNames 
    
    # Format as a list
    arroQuad <- list( # arroTo <- 
        "B" = bArrows, 
        "L" = lArrows, 
        "R" = rArrows  
    )   
    names( arroQuad ) <- .blrNames
    # names( arroTo )   <- .blrNames
    # rm( bArrows, lArrows, rArrows )
    
    ## Calculate the grid-lines coordinates for each axis
    ## Using a geometry specific method
    arroQuad <- ._ternaryAxisArrowsBase(
        s           = s, 
        .fracSum    = .fracSum, 
        arroQuad    = arroQuad, 
        tScale      = tScale, 
        arrowsShift = arrowsShift 
    )   
    
    return( arroQuad ) 
}   


## #INTERNAL. Geometry specific methods for .ternaryAxisArrowsBase() 
## #
## #INTERNAL. Geometry specific methods for .ternaryAxisArrowsBase() 
## #
## #
## #@param s 
## #  See .ternaryAxisArrowsBase() 
## #
## #@param \dots
## #  See .ternaryAxisArrowsBase() 
## #
## # 
## #@rdname _ternaryAxisArrowsBase-methods
## #
## #@export 
## #
## #@keywords internal
## #
._ternaryAxisArrowsBase <- function(
    s, 
    .fracSum, 
    arroQuad, 
    tScale, 
    arrowsShift 
){  
    UseMethod( "._ternaryAxisArrowsBase" ) 
}   


## #@rdname _ternaryAxisArrowsBase-methods
## #
## #@method ._ternaryAxisArrowsBase geo_TTT
## #
## #@export
## #
._ternaryAxisArrowsBase.geo_TTT <- function( 
    s, 
    .fracSum, 
    arroQuad, 
    tScale, 
    arrowsShift 
){  
    ## Calculate the grid-lines coordinates for each axis
    for( ax in 1:3 ){ 
        # Index of previous and next axis 
        if( ax == 1 ){ 
            axPrev <- 3 
            axNext <- 2 
        }else if( ax == 2 ){ 
            axPrev <- 1 
            axNext <- 3 
        }else if( ax == 3 ){ 
            axPrev <- 2 
            axNext <- 1 
        }   
        
        # Start coordinates on next axis is 0 or min
        arroQuad[[ ax ]][, axNext ] <- tScale[ "min", axNext ] - 
            arrowsShift[ c( 2, 2, 1, 2 ) ] * .fracSum 
        
        # Start coordinates on previous axis
        arroQuad[[ ax ]][, axPrev ] <- 
            .fracSum - 
            arroQuad[[ ax ]][, ax ] - 
            arroQuad[[ ax ]][, axNext ]
    }   
    
    return( arroQuad ) 
}   


## #@rdname _ternaryAxisArrowsBase-methods
## #
## #@method ._ternaryAxisArrowsBase geo_FFF
## #
## #@export
## #
._ternaryAxisArrowsBase.geo_FFF <- function( 
    s, 
    .fracSum, 
    arroQuad, 
    tScale, 
    arrowsShift 
){  
    ## Calculate the grid-lines coordinates for each axis
    for( ax in 1:3 ){ 
        # Index of previous and next axis 
        if( ax == 1 ){ 
            axPrev <- 3 
            axNext <- 2 
        }else if( ax == 2 ){ 
            axPrev <- 1 
            axNext <- 3 
        }else if( ax == 3 ){ 
            axPrev <- 2 
            axNext <- 1 
        }   
        
        # Start coordinates on previous axis is 0 or min
        arroQuad[[ ax ]][, axPrev ] <- tScale[ "min", axPrev ] - 
            arrowsShift[ c( 2, 2, 1, 2 )] * .fracSum 
        
        # Start coordinates on next axis
        arroQuad[[ ax ]][, axNext ] <- 
            .fracSum - 
            arroQuad[[ ax ]][, ax ] - 
            arroQuad[[ ax ]][, axPrev ]
    }   
    
    return( arroQuad ) 
}   


## #@rdname _ternaryAxisArrowsBase-methods
## #
## #@method ._ternaryAxisArrowsBase geo_FTX
## #
## #@export
## #
._ternaryAxisArrowsBase.geo_FTX <- function( 
    s, 
    .fracSum, 
    arroQuad, 
    tScale, 
    arrowsShift 
){  
    # Calculations axis by axis
    # ======================================================
    
    # Axis 1: bottom (counter-clockwise)
    # ------------------------------------------------------
    
    # Start coordinates on next axis is 0 or min
    arroQuad[[ 1L ]][, 2L ] <- tScale[ "min", 2L ] - 
        arrowsShift[ c( 2, 2, 1, 2 )] * .fracSum 
    
    # Start coordinates on previous axis
    arroQuad[[ 1L ]][, 3L ] <- 
        .fracSum - 
        arroQuad[[ 1L ]][, 1L ] - 
        arroQuad[[ 1L ]][, 2L ]
    
    
    # Axis 2: left (clockwise)
    # ------------------------------------------------------
    
    # Start coordinates on previous axis is 0 or min
    arroQuad[[ 2L ]][, 1L ] <- tScale[ "min", 1L ] - 
        arrowsShift[ c( 2, 2, 1, 2 ) ] * .fracSum 
    
    # Start coordinates on next axis
    arroQuad[[ 2L ]][, 3L ] <- 
        .fracSum - 
        arroQuad[[ 2L ]][, 2L ] - 
        arroQuad[[ 2L ]][, 1L ]
    
    
    # Axis 3: right (NA)
    # ------------------------------------------------------
    
    arroQuad[[ 3L ]][, 1L ] <- c( 
        NA, 
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5),  # /2
        .fracSum/2 + arrowsShift[ 1 ] * (.fracSum/2.0),  # /2
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5) ) # /2
        
    arroQuad[[ 3L ]][, 2L ] <- c( 
        NA, 
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5),  # /2
        .fracSum/2 + arrowsShift[ 1 ] * (.fracSum/2.0),    # /2
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5) ) # /2
    
    arroQuad[[ 3L ]][, 3L ] <- c( 
        NA, 
        -arrowsShift[ 2 ] * ((.fracSum/1.5)*2), 
        -arrowsShift[ 1 ] * .fracSum,
        -arrowsShift[ 2 ] * ((.fracSum/1.5)*2) ) 
    
    return( arroQuad ) 
}   


## #@rdname _ternaryAxisArrowsBase-methods
## #
## #@method ._ternaryAxisArrowsBase geo_TXF
## #
## #@export
## #
._ternaryAxisArrowsBase.geo_TXF <- function( 
    s, 
    .fracSum, 
    arroQuad, 
    tScale, 
    arrowsShift 
){  
    # Calculations axis by axis
    # ======================================================
    
    # Axis 1: bottom (clockwise)
    # ------------------------------------------------------

    # Start coordinates on previous axis is 0 or min
    arroQuad[[ 1L ]][, 3L ] <- tScale[ "min", 3L ] - 
        arrowsShift[ c( 2, 2, 1, 2 ) ] * .fracSum 
    
    # Start coordinates on next axis
    arroQuad[[ 1L ]][, 2L ] <- 
        .fracSum - 
        arroQuad[[ 1L ]][, 1L ] - 
        arroQuad[[ 1L ]][, 3L ]  
    
    # Axis 2: left (NA)
    # ------------------------------------------------------
    
    arroQuad[[ 2L ]][, 1L ] <- c( 
        NA, 
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5),  # /2
        .fracSum/2 + arrowsShift[ 1 ] * (.fracSum/2.0),  # /2
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5) ) # /2
        
    arroQuad[[ 2L ]][, 2L ] <- c( 
        NA, 
        -arrowsShift[ 2 ] * ((.fracSum/1.5)*2), 
        -arrowsShift[ 1 ] * .fracSum,
        -arrowsShift[ 2 ] * ((.fracSum/1.5)*2) )
    
    arroQuad[[ 2L ]][, 3L ] <- c( 
        NA, 
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5),  # /2
        .fracSum/2 + arrowsShift[ 1 ] * (.fracSum/2.0),  # /2
        .fracSum/2 + arrowsShift[ 2 ] * (.fracSum/1.5) ) # /2
    
    # Axis 3: right (counter-clockwise)
    # ------------------------------------------------------
    
    # Start coordinates on next axis is 0 or min
    arroQuad[[ 3L ]][, 1L ] <- tScale[ "min", 1L ] - 
        arrowsShift[ c( 2, 2, 1, 2 )] * .fracSum 
    
    # Start coordinates on previous axis
    arroQuad[[ 3L ]][, 2L ] <- 
        .fracSum - 
        arroQuad[[ 3L ]][, 3L ] - 
        arroQuad[[ 3L ]][, 1L ]
    
    
    return( arroQuad ) 
}   



# .ternaryAxisArrows ============================================

calculateArrowLength <- function( pin = NULL ){
    if( is.null( pin ) ){
        pin <- par( "pin" )
    }   
    
    #   Default arrow length in inches
    arrowLengthDefault <- 0.25 
    
    #   Default maximum plot dimension in inches
    maxPlotDim <- 7.545416
    
    #   Actual maximum plot dimension in inches
    pin <- max( pin ) 
    
    #   Desired arrow length
    return( arrowLengthDefault * (pin / maxPlotDim) )
}   

#' INTERNAL: Draw axis' arrows and arrows' label marks on a 
#'  triangle plot
#'
#' INTERNAL: Draw axis' arrows and arrows' label marks on a 
#'  triangle plot
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}} object.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#' 
#'@return
#'  Invisibly returns a list of \code{data.frame} 
#'  with the start and end points of the arrows segments 
#'  for each of the 3 axis.
#'
#' 
#'@rdname ternaryAxisArrows-methods
#'
#'@export 
#'
#'@keywords internal
#'
.ternaryAxisArrows <- function( s, ... ){  
    UseMethod( ".ternaryAxisArrows" ) 
}   


#'@rdname ternaryAxisArrows-methods
#'
#'@method .ternaryAxisArrows ternarySystem
#'
#'@export
#'
.ternaryAxisArrows.ternarySystem <- function( 
 s, 
 ... 
){  
    # Calculates the tick-marks and grid-segments position
    
    gr <- .ternaryAxisArrowsBase( s = s ) 
    
    .tlrAngle       <- tlrAngles( s = s ) 
    blrLabelAngles  <- c( 0, .tlrAngle[2], .tlrAngle[3] ) 
    
    # Change sign for the case when blrClock(s) is not TRUE NA FALSE
    if( !is.na( blrClock(s)[2] ) ){ 
        blrLabelAngles[3] <- -blrLabelAngles[3]
    }   
    
    
    pr <- tpPar( par = c( "arrowsBreak", "axis.line.lwd", 
        "arrowsLength" ) ) # "arrowsShift", "arrowsHeight", 
    .par <- par()
    
    arrowsBreak   <- pr$"arrowsBreak" 
    # arrowsShift   <- pr$"arrowsShift" 
    arrowsLength  <- pr$"arrowsLength" 
    fg            <- .par[[ "fg" ]] # par( "fg" )
    axis.line.lwd <- pr$"axis.line.lwd"
    col.lab       <- .par[[ "col.lab" ]] # par( "col.lab" )
    .fracSum      <- fracSum( s = s )
    
    
    # if( any( is.na( arrowsShift ) ) ){
        # #   Note: also set in .ternaryAxisArrowsBase
        # arrowsHeight <- pr$"arrowsHeight"
        # mgp          <- .par[[ "mgp" ]]
        
        # arrowsShift <- (.nbMargin2diffXY() / .fracSum) * 
            # c( mgp[ 1L ] - arrowsHeight, mgp[ 1L ] ) 
        
        # # print( arrowsShift ) 
    # }   
    
    
    if( is.na( arrowsLength ) ){
        arrowsLength <- calculateArrowLength( pin = .par[[ "pin" ]] )
    }   
    
    
    #   Chose the right adjustment
    # adj1 <- .ternaryClockSwitch( 
        # s   = s, 
        # ttt = c(  1,  0,     0    ), 
        # txf = c(  1,  1.15,  0    ), 
        # ftx = c(  0,  0,    -0.15 ), 
        # fff = c(  0,  1,     1    ) 
    # )   
    
    adj1 <- ._ternaryAxisArrows( s = s ) 
    
    for( ax in 1:length( gr ) ){ 
        # Draw the tick-marks start and segments
        if( nrow( gr[[ ax ]] ) != 0 ){ 
            # Prevent tests
            oldPar <- tpPar( par = "testRange" ) 
            tpPar  <- tpPar( testRange = FALSE ) 
            
            # ternaryPoints( 
                # x   = gr[[ ax ]], 
                # s   = s, 
                # col = "blue" )  
            
            if( !any( is.na( gr[[ ax ]][ 1, ] ) ) ){ 
                if( arrowsBreak ){ 
                    ternarySegments( 
                        from = gr[[ ax ]][ 1, ], 
                        to   = gr[[ ax ]][ 2, ], 
                        s    = s, 
                        col  = fg, 
                        lwd  = axis.line.lwd, 
                        ... ) 
                }else{ 
                    ternaryArrows( 
                        from   = gr[[ ax ]][ 1, ], 
                        to     = gr[[ ax ]][ 2, ], 
                        s      = s, 
                        col    = fg, 
                        lwd    = axis.line.lwd, 
                        length = arrowsLength, # diff( arrowsShift ), 
                        ... ) 
                }   
            }   
            
            # Draw the arrows' 2nd segments
            if( arrowsBreak | any( is.na( gr[[ ax ]][ 1, ] ) ) ){ 
                ternaryArrows( 
                    from   = gr[[ ax ]][ 2, ], 
                    to     = gr[[ ax ]][ 3, ], 
                    s      = s, 
                    col    = fg, 
                    lwd    = axis.line.lwd, 
                    length = arrowsLength, # diff( arrowsShift ), 
                    ... ) 
            }   
            
            
            # Add the axis / arrows labels
            
            # ternaryPoints( 
                # x      = gr[[ ax ]][ 4, ], 
                # s      = s, 
                # col    = "red", 
                # ... ) 
            
            ternaryText( 
                x      = gr[[ ax ]][ 4, ], 
                labels = s[[ 'ternaryVariables' ]][[ 'blrLabels' ]][ ax ], 
                s      = s, 
                # pos  = 2, 
                adj    = c( adj1[ ax ], .5 ), 
                srt    = blrLabelAngles[ ax ], 
                col    = col.lab, 
                ... ) 
            
            # Set test again
            tpPar( par = oldPar )
        }   
    }   
    
    return( invisible( gr ) ) 
}   


## # INTERNAL: Geometry specific parameters for .ternaryAxisArrows() 
## #
## # INTERNAL: Geometry specific parameters for .ternaryAxisArrows() 
## #
## #
## #@param s 
## #  See .ternaryAxisArrows() 
## #
## # 
## #@return
## #  See .ternaryAxisArrows() 
## #
## # 
## #@rdname _ternaryAxisArrows-methods
## #
## #@export 
## #
## #@keywords internal
## #
._ternaryAxisArrows <- function( s ){  
    UseMethod( "._ternaryAxisArrows" ) 
}   


## #@rdname _ternaryAxisArrows-methods
## #
## #@method ._ternaryAxisArrows geo_TTT
## #
## #@export
## #
._ternaryAxisArrows.geo_TTT <- function( 
 s  
){  
    return( c( 1, 0, 0 ) )
}   


## #@rdname _ternaryAxisArrows-methods
## #
## #@method ._ternaryAxisArrows geo_FFF
## #
## #@export
## #
._ternaryAxisArrows.geo_FFF <- function( 
 s  
){  
    return( c( 0, 1, 1 ) )
}   


## #@rdname _ternaryAxisArrows-methods
## #
## #@method ._ternaryAxisArrows geo_FFF
## #
## #@export
## #
._ternaryAxisArrows.geo_FTX <- function( 
 s  
){  
    return( c( 0, 0, -0.15 ) )
}   


## #@rdname _ternaryAxisArrows-methods
## #
## #@method ._ternaryAxisArrows geo_FFF
## #
## #@export
## #
._ternaryAxisArrows.geo_TXF <- function( 
 s  
){  
    return( c( 1, 1.15, 0 ) )
}   




# ternaryAxis ==================================================

#'Add axis to a ternary plot (axis lines, ticks, labels, titles and arrows)
#'
#'Add axis to a ternary plot (axis lines, ticks, labels, titles and arrows)
#'
#'
#'@param s 
#'  A \code{ternarySystem} object, as created with 
#'  \code{\link[ternaryplot]{createTernarySystem}}, or a single 
#'  \code{character} string. Can be missing.
#'
#'@param side
#'  A vector of integer specifying which side of the plot the axis is to
#'  be drawn on.  The axis is placed as follows: 1=below, 2=left, 3=right (note 
#'  that this differ from \code{\link[graphics]{axis}} where 3=above and 
#'  4=right). Default is to draw axis on the 3 sides.
#'
#'@param tick
#'  A logical value specifying whether tickmarks and an axis line
#'  should be drawn.
#'
#'@param arrow
#'  A logical value specifying whether axis' arrows should be drawn.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#' 
#'@rdname ternaryAxis-methods
#'
#'@export 
#'
ternaryAxis <- function( 
 s, 
 ... 
){  
    if( missing(s) ){ 
        UseMethod( "ternaryAxis", object = character(0) ) 
    }else{ 
        UseMethod( "ternaryAxis" ) 
    }   
}   



#'@rdname ternaryAxis-methods
#'
#'@method ternaryAxis character
#'
#'@export
#'
ternaryAxis.character <- function(
 s, 
 ... 
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    ternaryAxis( s = s, ... ) 
}   



#'@rdname ternaryAxis-methods
#'
#'@method ternaryAxis ternarySystem
#'
#'@export
#'
ternaryAxis.ternarySystem <- function(
 s, 
 side = 1:3,
 tick = TRUE,
 arrow = TRUE,
 # tickLabel = TRUE, 
 # axisTitle = TRUE, 
 ... 
){  
    .ternaryTicks( s = s ) 
    # ternaryGrid( s = s ) 
    # ternaryBox( s = s ) 
    .ternaryAxisArrows( s = s ) 
}   



