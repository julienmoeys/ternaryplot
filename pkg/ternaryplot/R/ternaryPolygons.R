
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
#'  Either a \code{\link[ternaryplot]{ternaryPolygons}}-object, 
#'  such as created by \code{\link[ternaryplot]{createTernaryGrid}} 
#'  or by \code{\link[ternaryplot]{ternaryClasses}}, or 
#'  a \code{ternarySystem}, such as obtained with 
#'  \code{\link[ternaryplot]{getTernarySystem}} or output 
#'  by \code{\link[ternaryplot]{ternaryPlot}}.
#'
#'@param x 
#'  Not used.
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
#'  of the polygons. If \code{NULL}, the default label 
#'  in \code{s[[ 'labels' ]]} are used (When \code{s} is a #
#'  \code{ternaryPolygons}-object). If a single character 
#'  string, recycled for each polygon. Set to \code{NA} to 
#'  prevent any label to be drawn.
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
ternaryPolygons.ternaryPolygons <- function( 
    s, 
    x       = NULL, 
    density = NULL, 
    angle   = 45,
    border  = NULL, 
    bg      = NULL, 
    lty     = par("lty"), 
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
    ... 
){  
    if( !is.null( x ) ){
        warning( "'x' is not NULL. Ignored by ternaryPolygons.ternaryPolygons()." )
    }   
    
    terSys  <- ternarySystem( s = s ) 
    
    # x  <- s[[ "grid" ]] 
    
    if( is.null( labels ) ){
        polLab <- attr( x = s, which = "labels" )
        
        if( !is.null( polLab ) ){
            labels <- polLab
        }   
        
        rm( polLab )
    }   
    
    if( nrow( s ) > 0 ){
        # #   Correspondence between labels and polygons IDs
        # if( !is.null( labels ) ){
            # names( labels ) <- as.character( unique( s[, "id"] ) ) 
        # }
        
        # s  <- s[ order( s[, "id"] ), ] 
        
        idCol <- attr( x = s, which = "idCol" ) 
        
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
        
        .par <- par( no.readonly = TRUE )
        
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
                polygon(
                    x       = xy[[ i ]][, "x" ], 
                    y       = xy[[ i ]][, "y" ], 
                    density = density[ i ], 
                    angle   = angle[ i ], 
                    border  = border[ i ], 
                    col     = bg[ i ], 
                    lty     = lty[ i ], 
                    lwd     = lwd[ i ], 
                    cex     = cex, 
                    font    = font, 
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
        .par <- par( no.readonly = TRUE )
        
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


