
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



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
    .blrNames    <- blrNames.ternarySystem( s = s )  
    .blrClock    <- blrClock.ternarySystem( s )  
    tScale       <- s[[ 'scale' ]] 
    .fracSum     <- fracSum.ternarySystem( s = s ) 
    marginSize   <- .nbMargin2diffXY()
    .par         <- par() 
    .tpPar       <- tpPar() 
    
    if( type == "ticks" ){ 
        # ticksShiftTo <- getTpPar( "ticksShift" ) 
        ticksShift <- .tpPar[[ "ticksShift" ]] # getTpPar( "ticksShift" ) 
        
        if( is.na( ticksShift ) ){
            ticksShift <- (marginSize / .fracSum) * (-1 * .par[[ "tcl" ]])
        }   
        
        ticksShiftFrom <- (marginSize / .fracSum) * .par[[ "mgp" ]][ 3L ] 
        ticksShiftTo   <- ticksShiftFrom + ticksShift
        
    }else if( type == "tickLabels" ){
        ticksLabelsShift <- .tpPar[[ "ticksLabelsShift" ]] # getTpPar( "ticksLabelsShift" ) 
        
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
    bTicks <- lTicks <- rTicks <- .tpPar[[ "ticksAt" ]] * .fracSum # getTpPar( "ticksAt" )
    
    
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
    s <- .generateTernaryGeometry2ndClass( 
        s      = s, 
        class1 = "ternarySystem" 
    )   
    
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
    .fracSum     <- fracSum.ternarySystem( s = s ) 
    
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
    .fracSum     <- fracSum.ternarySystem( s = s ) 
    
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
    .fracSum     <- fracSum.ternarySystem( s = s ) 
    
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
    .fracSum     <- fracSum.ternarySystem( s = s ) 
    
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
 ... 
){  
    # Calculates the tick-marks and grid-segments position
    gr <- .ternaryGridBase( s = s, type = "grid" ) 
    
    .tpPar        <- tpPar() 
    grid.line.col <- .tpPar[[ "grid.line.col" ]] # getTpPar( "grid.line.col" )
    grid.line.lwd <- .tpPar[[ "grid.line.lwd" ]] # getTpPar( "grid.line.lwd" )
    
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
            
            if( is.null( grid.line.lwd ) ){ 
                grid.line.lwd <- par( "lwd" ) 
            }   
            
            out[[ ax ]] <- ternarySegments.ternarySystem( 
                s     = s, 
                from  = gr[[ "from" ]][[ ax ]], 
                to    = gr[[ "to" ]][[ ax ]], 
                col   = grid.line.col, 
                lwd   = grid.line.lwd, 
                ... ) 
        }   
    }   
    
    
    # if( getTpPar( "sp" ) ){ 
        # isNull <- unlist( lapply( X = out, FUN = is.null ) )
        
        # out <- do.call( what = "rbind.SpatialLines", 
            # args = c( out[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        # # spChFIDs( out ) <- c( "B", "L", "R" )[ !isNull ]
    # }   
    
    return( invisible( out ) ) 
}   


