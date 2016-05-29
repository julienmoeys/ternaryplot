
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

# Useful: \code{} \code{\link[]{}} 



# ternaryText ===================================================

#' Add Text to a ternary plot
#'
#' Add Text to a ternary plot
#'
#'
#'@seealso
#'  \code{\link[graphics]{text}}.
#'
#'
#'@param s  
#'  A \code{\link[ternaryplot]{ternarySystem}} object, or a 
#'  character string naming a pre-defined \code{\link[ternaryplot]{ternarySystem-class}}. 
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
#'@param adj  
#'  See \code{\link[graphics]{text}}.
#'
#'@param pos  
#'  See \code{\link[graphics]{text}}.
#'
#'@param offset  
#'  See \code{\link[graphics]{text}}.
#'
#'@param vfont  
#'  See \code{\link[graphics]{text}}.
#'
#'@param cex  
#'  See \code{\link[graphics]{text}}.
#'
#'@param col  
#'  See \code{\link[graphics]{text}}.
#'
#'@param font  
#'  See \code{\link[graphics]{text}}.
#'
#'@param what 
#'  Single character string. When \code{x} is a missing 
#'  and \code{s} is a \code{\link[ternaryplot]{ternarySystem-class}}, what points 
#'  should be extracted: the class vertices 
#'  (\code{what = 'vertices'}, the default) 
#'  or the class centroids 
#'  (\code{what = 'centroids'})
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{text}}.
#'
#'
#'@return
#'  Invisibly returns the graphical element as x-y coordinates.
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
    adj     = NULL,
    pos     = NULL, 
    offset  = 0.5, 
    vfont   = NULL,
    cex     = 1, 
    col     = NULL, 
    font    = NULL, 
    what    = "centroids", 
    ... 
){  
    if( missing( "x" ) ){
        if( what == "vertices" ){
            pts <- s[[ "vertices" ]]
            
            if( !is.null( pts ) ){
                xy <- ternary2xy.ternarySystem( s = s, x = pts ) 
                
                # xy <- xy[, c( "x", "y" ) ] 
                
                # xy <- sp::SpatialPoints( coords = xy ) 
                
                if( missing( "labels" ) ){
                    labels <- pts[, "id" ] 
                }   
                
            }else{
                xy <- NULL 
            }   
            
        }else if( what == "centroids" ){
            pol <- ternary2SpatialPolygons( s = s ) 
            
            if( !is.null( pol ) ){
                xy <- sp::coordinates( pol ) 
                
                colnames( xy ) <- c( "x", "y" )
                
                if( missing( "labels" ) ){
                    labels <- s[[ "classes" ]][, "abbrev" ] 
                }   
                
                # xy <- sp::SpatialPoints( coords = xy ) 
            }else{
                xy <- NULL 
            }   
        }else{
            stop( sprintf( "Unknown / unsupported value for argument 'what' (%s)", what ) )
        }   
    }else{
        xy <- ternary2xy.ternarySystem( s = s, x = x ) 
    }   
    
    if( !is.null( xy ) ){
        text( x = xy[,"x"], y = xy[,"y"], labels = labels, 
            adj = adj, pos = pos, offset = offset, vfont = vfont, 
            cex = cex, col = col, font = font, ... ) 
        
        out <- xy[, c( "x", "y" ) ]
    }   
    
    # if( getTpPar( "sp" ) ){ out <- SpatialPoints( coords = out ) }
    
    return( invisible( out ) ) 
}   



#'@rdname ternaryText-methods
#'
#'@method ternaryText ternaryPolygons
#'
#'@export
#'
#'@importFrom sp coordinates 
ternaryText.ternaryPolygons <- function( 
    s, 
    x, 
    labels, 
    adj     = NULL,
    pos     = NULL, 
    offset  = 0.5, 
    vfont   = NULL,
    cex     = 1, 
    col     = NULL, 
    font    = NULL, 
    ... 
){  
    if( !missing( "x" ) ){
        warning( "'x' is not NULL. Ignored by ternaryText.ternaryPolygons()." )
    }   
    
    # terSys  <- ternarySystem( s = s ) 
    
    # x  <- s[[ "grid" ]] 
    
    if( is.null( labels ) ){
        polLab <- attr( x = s, which = "labels" )
        
        if( !is.null( polLab ) ){
            labels <- polLab
        };  
        
        rm( polLab )
    }   
    
    #   Draw the labels
    if( !is.null( labels ) ){
        idCol <- attr( x = s, which = "idCol" ) 
        
        #   Number of unique classes
        nbCl <- length( unique( s[, idCol ] ) ) # s[[ "grid" ]][, idCol ]
        
        
        if( (length( labels ) == 1) & (length( nbCl ) > 1) ){
            labels <- rep( labels, times = length( nbCl ) )
        }   
        
        
        if( !all( is.na( labels ) ) ){
            #   Convert the polygons to SpatialPolygonsDataFrame
            sSp <- ternary2SpatialPolygons.ternaryPolygons( s = s ) 
            
            #   Convert to x-y points, that is the polygons 
            #   centroid
            centroids <- sp::coordinates( sSp ) 
            
            text( x = centroids[, 1L ], y = centroids[, 2L ], 
                labels = labels, adj = adj, pos = pos, 
                offset = offset, vfont = vfont, cex = cex, 
                col = col, font = font, ... ) 
        }else{
            centroids <- NULL 
        }   
    }else{
        centroids <- NULL 
    }   
    
    return( invisible( centroids ) ) 
}   


