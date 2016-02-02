
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



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
    
    
    .par   <- par() 
    .tpPar <- tpPar() 
    
    fg            <- .par[[ "fg" ]]
    
    col.lab       <- .par[[ "col.lab" ]] 
    font.lab      <- .par[[ "font.lab" ]] 
    
    axis.line.lwd <- .tpPar[[ "axis.line.lwd" ]] 
    axis.line.col <- .tpPar[[ "axis.line.col" ]] 
    
    ticks.line.lwd <- .tpPar[[ "ticks.line.lwd" ]] 
    ticks.line.col <- .tpPar[[ "ticks.line.col" ]] 
    
    if( is.null( axis.line.lwd ) ){
        axis.line.lwd <- .par[[ "lwd" ]] 
    }   
    
    if( is.null( axis.line.col ) ){
        axis.line.col <- .par[[ "fg" ]] 
    }   
    
    if( is.null( ticks.line.lwd ) ){
        ticks.line.lwd <- .par[[ "lwd" ]] 
    }   
    
    if( is.null( ticks.line.col ) ){
        ticks.line.col <- .par[[ "fg" ]] 
    }   
    
    #   Geometry specific adjustments
    s <- .generateTernaryGeometry2ndClass( 
        s      = s, 
        class1 = "ternarySystem" 
    )   
    
    adj <- ._ternaryTicks( s = s ) 
    
    
    for( ax in side ){ # 
        # Draw the tick-marks start and segments
        if( nrow( grTm[[ "from" ]][[ ax ]] ) != 0 ){ 
            # ternaryPoints( 
                # x   = grTm[[ "from" ]][[ ax ]], 
                # s   = s )  
            
            # Prevent tests
            oldPar <- tpPar( par = "testRange" )        
            tpPar  <- tpPar( testRange = FALSE ) 
            
            # Draw the ticks segments
            out[[ ax ]] <- ternarySegments( 
                from  = grTm[[ "from" ]][[ ax ]], 
                to    = grTm[[ "to" ]][[ ax ]], 
                s     = s, 
                col   = ticks.line.col, 
                lwd   = ticks.line.lwd, 
                ... )  
            
            # Draw the axis line
            outLine[[ ax ]] <- ternarySegments( 
                from  = grTm[[ "from" ]][[ ax ]][ 1L, , drop = FALSE ], 
                to    = grTm[[ "from" ]][[ ax ]][ nrow( grTm[[ "from" ]][[ ax ]] ), , drop = FALSE ], 
                s     = s, 
                col   = axis.line.col, 
                lwd   = axis.line.lwd, 
                ... )  
            
            ternaryText( 
                x      = grTl[[ "to" ]][[ ax ]], 
                labels = as.character( grTl[[ "to" ]][[ ax ]][, ax ] ), 
                s      = s, 
                # pos  = 2, 
                adj    = c( adj[[ "adj1" ]][ ax ], adj[[ "adj2" ]][ ax ] ), 
                srt    = adj[[ "blrLabelAngles" ]][ ax ], 
                col    = col.lab, 
                font   = font.lab, 
                # offset = -5, 
                ... ) 
            
            # Set test again
            tpPar( "testRange" = oldPar[[ "testRange" ]] ) 
        }   
    }   
    
    
    # if( getTpPar( "sp" ) ){ 
        # isNull <- unlist( lapply( X = out, FUN = is.null ) )
        
        # out <- do.call( what = "rbind.SpatialLines", 
            # args = c( out[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        # isNull <- unlist( lapply( X = outLine, FUN = is.null ) )
        
        # outLine <- do.call( what = "rbind.SpatialLines", 
            # args = c( outLine[ !isNull ], list( "makeUniqueIDs" = TRUE ) ) ) 
        
        # out <- sp::rbind.SpatialLines( out, outLine, makeUniqueIDs = TRUE )
        
        # # spChFIDs( out ) <- c( "B", "L", "R" )[ !isNull ]
    # }   
    
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
    s <- .generateTernaryGeometry2ndClass( 
        s      = s, 
        class1 = "ternarySystem" 
    )   
    
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
    
    
    .tpPar <- tpPar() # "arrowsShift", "arrowsHeight", 
    .par   <- par()
    
    arrowsBreak   <- .tpPar[[ "arrowsBreak" ]] 
    # arrowsShift   <- .tpPar$"arrowsShift" 
    arrowsLength  <- .tpPar[[ "arrowsLength" ]]
    # fg          <- .par[[ "fg" ]] # par( "fg" )
    axis.line.col <- .tpPar[[ "axis.line.col" ]] 
    axis.line.lwd <- .tpPar[[ "axis.line.lwd" ]]
    col.lab       <- .par[[ "col.lab" ]] # par( "col.lab" )
    .fracSum      <- fracSum( s = s )
    
    if( is.null( axis.line.col ) ){
        axis.line.col <- .par[[ "fg" ]] 
    }   
    
    if( is.null( axis.line.lwd ) ){
        axis.line.lwd <- .par[[ "lwd" ]] 
    }   
    
    # if( any( is.na( arrowsShift ) ) ){
        # #   Note: also set in .ternaryAxisArrowsBase
        # arrowsHeight <- .tpPar$"arrowsHeight"
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
    
    s <- .generateTernaryGeometry2ndClass( 
        s      = s, 
        class1 = "ternarySystem" 
    )   
    
    adj1 <- ._ternaryAxisArrows( s = s ) 
    
    
    for( ax in 1:length( gr ) ){ 
        # Draw the tick-marks start and segments
        if( nrow( gr[[ ax ]] ) != 0 ){ 
            # Prevent tests
            # oldPar <- tpPar( par = "testRange" ) 
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
                        col  = axis.line.col, 
                        lwd  = axis.line.lwd, 
                        ... ) 
                }else{ 
                    ternaryArrows( 
                        from   = gr[[ ax ]][ 1, ], 
                        to     = gr[[ ax ]][ 2, ], 
                        s      = s, 
                        col    = axis.line.col, 
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
                    col    = axis.line.col, 
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
                font   = par( "font.axis" ), 
                ... ) 
            
            # Set test again
            tpPar( par = .tpPar )
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
    oldPar <- par( "xpd" = TRUE ) # Plotting can also occur out of the plot
    
    .ternaryTicks( s = s ) 
    
    # ternaryGrid( s = s ) 
    # ternaryBox( s = s ) 
    
    if( getTpPar( "arrows" ) ){
        .ternaryAxisArrows( s = s ) 
    }   
    
    par( "xpd" = oldPar[[ "xpd" ]] ) 
}   


