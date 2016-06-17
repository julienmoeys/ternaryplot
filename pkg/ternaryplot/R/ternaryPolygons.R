
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryPolygons ===============================================

#'Add polygons (a grid or ternary-classes) to an existing ternary plot
#'
#'Add polygons (a grid or ternary-classes) to an existing ternary 
#'  plot
#'
#'
#'@seealso \code{\link[ternaryplot]{ternaryPlot}} (or 
#'  \code{\link[ternaryplot]{ternaryWindow}}) to create 
#'  a plot (window) on which the ternary polygons can 
#'  be added. \code{\link[graphics]{polygon}} for the equivalent 
#'  non-ternary function.
#'
#'
#'@param s 
#'  Either a \code{\link[ternaryplot]{ternaryPolygons-class}} 
#'  object, such as created by 
#'  \code{\link[ternaryplot]{createTernaryGrid}} 
#'  or by \code{\link[ternaryplot]{ternaryClasses}}, or 
#'  a \code{\link[ternaryplot]{ternarySystem-class}}, such 
#'  as obtained with \code{\link[ternaryplot]{getTernarySystem}} 
#'  or output by \code{\link[ternaryplot]{ternaryPlot}}.
#'
#'@param z 
#'  Single character string. When \code{s} is a 
#'  \code{\link[ternaryplot]{ternaryPolygons-class}}, 
#'  name of a variable in \code{attr(s,"data")} to be displayed 
#'  with a colour gradient (the colour of each polygon is made 
#'  proportional to the values in \code{attr(s,"data")[,"z"]}).
#'  If \code{bg} is not \code{NULL}, \code{z} will be ignored 
#'  and a \code{\link[base]{warning}} is emitted.
#'
#'  See also the arguments \code{breaks}, \code{cut.args}, 
#'  \code{legend.add}, \code{legend.x}, \code{legend.title}, 
#'  \code{legend.args} and \code{noZero} below.
#'
#'@param density 
#'  See \code{\link[graphics]{polygon}}.
#'
#'@param angle
#'  See \code{\link[graphics]{polygon}}.
#'
#'@param border
#'  See \code{\link[graphics]{polygon}}.
#'
#'@param bg 
#'  Passed to \code{col} in \code{\link[graphics]{polygon}}.
#'
#'@param lty
#'  See \code{\link[graphics]{polygon}}.
#'
#'@param lwd
#'  See \code{\link[graphics]{par}}. Thickness of the polygon 
#'  border.
#'
#'@param labels
#'  Vector of character strings. Labels to be drawn on top 
#'  of the polygons. If \code{NULL}, the value in 
#'  \code{attr(s,'data')[,'abbrev']} are used, when present.
#'
#'@param adj  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param pos  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param offset  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param vfont  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'@param cex  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param col  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param font  
#'  See \code{\link[graphics]{text}} (parameter of the text 
#'  \code{labels}).
#'
#'@param breaks 
#'  Only used if \code{z} is not \code{NULL}. 
#'  See \code{\link[base]{cut}}. Break-values or number of 
#'  groups to be used when preparing the colour-scale and 
#'  the colour-scale legend for the binned values. See 
#'  also \code{cut.args} below.
#'
#'@param cut.args 
#'  Only used if \code{z} is not \code{NULL}. 
#'  If not \code{NULL}, a list of additional arguments to be 
#'  passed to \code{\link[base]{cut}} (see \code{breaks} 
#'  above). Values \code{x}, \code{breaks}, \code{dig.lab} 
#'  and \code{include.lowest} not allowed. \code{dig.lab} 
#'  is internally set to 0 (as counts are integer values) 
#'  and \code{include.lowest} is set to \code{TRUE} internally.
#'
#'@param legend.add 
#'  Only used if \code{z} is not \code{NULL}. 
#'  Single logical value. If \code{TRUE} (the default), 
#'  a legend is added on the plot.
#'
#'@param legend.x 
#'  Only used if \code{z} is not \code{NULL}. 
#'  See argument \code{x} in \code{\link[graphics]{legend}}. 
#'  Position of the legend. Default value is \code{topright} 
#'  unless \code{tlrAngles(s)[3L]} is higher than 60 degrees 
#'  (which indicates a ternary system with a right-angle 
#'  located at the right side of the plot), in which case 
#'  the value is set to \code{topleft} (set internally in 
#'  all cases).
#'
#'@param legend.title 
#'  Only used if \code{z} is not \code{NULL}. 
#'  See argument \code{title} in \code{\link[graphics]{legend}}. 
#'  Title of the legend.
#'
#'@param legend.args 
#'  Only used if \code{z} is not \code{NULL}. 
#'  If not \code{NULL}, a list of additional arguments to be 
#'  passed to \code{\link[graphics]{legend}}. Arguments 
#'  \code{x} and \code{title} are excluded and should be set 
#'  with \code{legend.x} and \code{legend.title}.
#'
#'@param noZero 
#'  Only used if \code{z} is not \code{NULL}. 
#'  Single logical value. If \code{TRUE} (the default), 
#'  zero-counts values are not shown on the plot. This also 
#'  makes the legend more readable.
#'
#'@param \dots
#'  Additional arguments passed to \code{\link[graphics]{polygon}}.
#'
#'
#'return 
#'  Invisibly returns a list of polygon coordinates in 
#'  xy coordinates.
#'
#'
#'@rdname ternaryPolygons-methods
#'
#'@export
#'
ternaryPolygons <- function( 
 s, 
 ... 
){  
    UseMethod( "ternaryPolygons" ) 
}   



#'@rdname ternaryPolygons-methods
#'
#'@method ternaryPolygons ternaryPolygons
#'
#'@export
#'
#'@importFrom graphics par
#'@importFrom graphics polygon
#'@importFrom graphics legend
ternaryPolygons.ternaryPolygons <- function( 
    s, 
    z       = NULL, 
    density = NULL, 
    angle   = 45,
    border  = NULL, 
    bg      = NULL, 
    lty     = graphics::par("lty"), 
    lwd     = NULL, 
    # ternaryText arguments
    labels  = NULL, 
    adj     = NULL,
    pos     = NULL, 
    offset  = 0.5, 
    vfont   = NULL,
    cex     = 1, 
    col     = NULL, 
    font    = NULL, 
    breaks       = 6L, 
    cut.args     = NULL, 
    legend.add   = TRUE, 
    legend.x     = NULL, 
    legend.title = "counts", 
    legend.args  = NULL, 
    noZero       = FALSE, 
    ... 
){  
    ternaryCheck.ternaryPolygons( s = s )
    
    terSys  <- ternarySystem( s = s ) 
    
    .tpPar <- tpPar()
    
    
    if( nrow( s ) > 0 ){
        idCol <- attr( x = s, which = "idCol" ) 
        data  <- attr( x = s, which = "data" ) 
                
        
        #   Treat the case where z is not NULL 
        #   (colour overlay)
        if( is.null( bg ) & (!is.null( z )) ){
            zPlot <- TRUE 
            
            if( is.null( data ) ){
                stop( "'z' is not NULL, but 's' has no attribute 'data' (data.frame where 'z' should be)" )
            }   

            if( !is.character(z) ){
                stop( sprintf( 
                    "'z' should be a character string (now %s)", 
                    paste( class( z ), collapse = "; " ) 
                ) ) 
            }   
            
            if( length(z) != 1L ){
                stop( sprintf( 
                    "length(z) should be 1 (now %s)", 
                    length(z)
                ) ) 
            }   
            
            if( !(z %in% colnames( data )) ){
                stop( sprintf( 
                    "Column %s ('z') cannot be found in attr(s,'data')", 
                    z 
                ) ) 
            }   
            
            #   Fetch the values to be used as overlay
            zValues <- data[, z ] 
            names( zValues ) <- as.character( data[, idCol ] ) 
            
            if( noZero ){
                zIsZero <- zValues == 0 
                
                zValues <- zValues[ !zIsZero ]
                data    <- data[ !zIsZero, ]
                
                #   Select only the polygons with a non zero 
                #   z-value
                s <- s[ s[, idCol ] %in% data[, idCol ], ]
                
                rm( zIsZero )
            }   
            
            #   Find out if these are all integers
            zInt <- zValues %% 1
            zInt <- all( zInt == 0 )
            
            if( length( breaks ) == 1L ){
                cRange <- range( zValues, na.rm = TRUE ) 
                breaks <- seq( 
                    from       = cRange[ 1L ],
                    to         = cRange[ 2L ],
                    length.out = breaks )
                
                rm( cRange )
                
                if( zInt ){
                    breaks <- round( breaks, digits = 0L ) 
                }   
                    
                breaks <- unique( breaks ) 
            }   
            
            if( zInt ){
                cut.args0 <- list( 
                    "x"              = zValues, 
                    "breaks"         = breaks, 
                    "dig.lab"        = 0L, 
                    "include.lowest" = TRUE 
                )   
            }else{
                cut.args0 <- list( 
                    "x"              = zValues, 
                    "breaks"         = breaks, 
                    "include.lowest" = TRUE 
                )   
            }   
            
            if( !is.null( cut.args ) ){
                if( !("list" %in% class( cut.args )) ){
                    stop( "If not NULL 'cut.args' must be a list" )
                }   
                
                cut.args0 <- c( cut.args0, cut.args ) 
            }   
            
            cut_z <- do.call( what = cut, args = cut.args0 )
            rm( cut.args0 )
            
            ternaryPolygons.bg.fun <- .tpPar[[ "ternaryPolygons.bg.fun" ]]
            
            .levels <- levels( cut_z ) 
            bg0     <- rev( ternaryPolygons.bg.fun( n = length( .levels ) ) ) 
            names( bg0 ) <- .levels 
            bg      <- bg0[ as.character( cut_z ) ]
            bg      <- as.character( bg ) 
            names( bg ) <- names( zValues ) 
            
            #   Align bg to the number of polygons
            bg <- as.character( bg[ as.character( unique( s[, idCol ] ) ) ] )
            
            rm( cut_z, ternaryPolygons.bg.fun, zInt, zValues ) 
            
            if( is.null( border ) ){
                border <- .tpPar[[ "ternaryPolygons.border.col" ]] 
            }   
        }else{
            zPlot <- FALSE 
        }   
        
        
        if( is.null( labels ) ){
            if( !is.null( data ) ){
                if( "abbrev" %in% colnames( data ) ){
                    labels <- data[, "abbrev" ] 
                }   
            }   
        }   
        
        
        # #   Correspondence between labels and polygons IDs
        # if( !is.null( labels ) ){
            # names( labels ) <- as.character( unique( s[, "id"] ) ) 
        # }
        
        # s  <- s[ order( s[, "id"] ), ] 
        
        id <- s[, idCol ] 
        # s  <- s[, colnames( s ) != idCol ] 
        
        .blrNames <- blrNames.ternarySystem( terSys ) 
        
        #   Transform from Top-Left-Right to X-Y
        xy <- ternary2xy.ternarySystem( s = terSys, x = s[, .blrNames ] ) 
        
        #   Factor version of IDs
        idf <- factor( x = id, levels = unique( id ), 
            labels = as.character( unique( id ) ) ) 
        
        xy  <- split( x = xy, f = idf ) 
        nxy <- names( xy ) 
        rm( idf )
        
        # #   Re-order the labels
        # if( !is.null( labels ) ){
            # labels <- as.character( labels[ nxy  ] )
        # }
        
        # if( !is.null( polygonExtra ) ){
            # if( !is.list( polygonExtra ) ){
                # stop( "'polygonExtra' must be a list (with names)" )
            # }   
        # }   
        
        .par <- graphics::par( no.readonly = TRUE )
        
        #   Recycle all the parameters for the number of 
        #   polygons
        if( !is.null( density ) ){
            if( (length( density ) == 1) & (length( nxy ) > 1) ){
                density <- rep( density, times = length( nxy ) )
            }   
        }   
        
        if( !is.null( angle ) ){
            if( (length( angle ) == 1) & (length( nxy ) > 1) ){
                angle <- rep( angle, times = length( nxy ) )
            }   
        }   
        
        if( !is.null( border ) ){
            if( (length( border ) == 1) & (length( nxy ) > 1) ){
                border <- rep( border, times = length( nxy ) )
            }   
        }   
        
        if( !is.null( bg ) ){
            if( (length( bg ) == 1) & (length( nxy ) > 1) ){
                bg <- rep( bg, times = length( nxy ) )
            }   
        }   
        
        if( !is.null( lty ) ){
            if( (length( lty ) == 1) & (length( nxy ) > 1) ){
                lty <- rep( lty, times = length( nxy ) )
            }   
        }   
        
        if( !is.null( lwd ) ){
            if( (length( lwd ) == 1) & (length( nxy ) > 1) ){
                lwd <- rep( lwd, times = length( nxy ) )
            }   
        }else{
            lwd <- rep( .par[[ "lwd" ]], times = length( nxy ) )
        }   
        
        if( !is.null( cex ) ){
            if( (length( cex ) == 1) & (length( nxy ) > 1) ){
                cex <- rep( cex, times = length( nxy ) )
            }   
        }else{
            cex <- rep( .par[[ "cex" ]], times = length( nxy ) )
        }   
        
        if( !is.null( font ) ){
            if( (length( font ) == 1) & (length( nxy ) > 1) ){
                font <- rep( font, times = length( nxy ) )
            }   
        }else{
            font <- rep( .par[[ "font" ]], times = length( nxy ) )
        }   
        
        silent <- lapply( 
            X   = 1:length( xy ), 
            FUN = function(i){ 
                graphics::polygon(
                    x       = xy[[ i ]][, "x" ], 
                    y       = xy[[ i ]][, "y" ], 
                    density = density[ i ], 
                    angle   = angle[ i ], 
                    border  = border[ i ], 
                    col     = bg[ i ], 
                    lty     = lty[ i ], 
                    lwd     = lwd[ i ], 
                    # cex   = cex, 
                    # font  = font, 
                    ...
                )   
            }   
        )   
        
        #   Draw the labels
        if( !is.null( labels ) ){
            if( !all( is.na( labels ) ) ){
                ternaryText.ternaryPolygons( 
                    s       = s, 
                    # x, 
                    labels  = labels, 
                    adj     = adj,
                    pos     = pos, 
                    offset  = offset, 
                    vfont   = vfont,
                    cex     = cex, 
                    col     = col, 
                    font    = font, 
                )   
            }   
        }   
        
        if( zPlot ){
            if( legend.add ){
                if( is.null( legend.x ) ){
                    if( tlrAngles( terSys )[ 3L ] <= 60 ){
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
                
                do.call( what = graphics::legend, args = legend.args0 )
            }   
        }   
        
    }else{
        warning( "No polygon to be plotted (empty 'ternaryPolygons')" )
        
        xy <- NULL 
    }   
    
    return( invisible( xy ) ) 
}   



#'@rdname ternaryPolygons-methods
#'
#'@method ternaryPolygons ternarySystem
#'
#'@export
#'
#'@importFrom graphics par
ternaryPolygons.ternarySystem <- function( 
    s, 
    border  = NULL, 
    col     = NULL, 
    bg      = NULL, 
    lwd     = NULL, 
    cex     = NULL, 
    font    = NULL, 
    ... 
){  
    if( nrow( s[[ "classes" ]] ) > 0 ){
        tc <- ternaryClasses( s = s ) 
        
        .tpPar <- tpPar() 
        .par   <- graphics::par( no.readonly = TRUE )
        
        if( is.null( border ) ){
            border <- .tpPar[[ "class.border.col" ]] 
        }   
        
        if( is.null( col ) ){
            col <- .tpPar[[ "class.label.col" ]] 
            
            if( is.null( col ) ){
                col <- .par[[ "col.lab" ]] 
            }   
        }   
        
        if( is.null( bg ) ){
            bg <- .tpPar[[ "class.bg" ]] 
        }   
        
        if( is.null( lwd ) ){
            lwd <- .tpPar[[ "class.border.lwd" ]] 
            
            if( is.null( lwd ) ){
                lwd <- .par[[ "lwd" ]] 
            }   
        }   
        
        if( is.null( cex ) ){
            cex <- .tpPar[[ "class.label.cex" ]] 
            
            if( is.null( cex ) ){
                cex <- .par[[ "cex.lab" ]] 
            }   
        }   
        
        if( is.null( font ) ){
            font <- .tpPar[[ "class.label.font" ]] 
            
            if( is.null( font ) ){
                font <- .par[[ "font.lab" ]]
            }   
        }   
        
        out <- ternaryPolygons.ternaryPolygons( s = tc, 
            bg = bg, col = col, border = border, lwd = lwd, 
            cex = cex, font = font, ... ) 
    }else{
        out <- NULL 
    }   
    
    return( invisible( out ) )
}   


