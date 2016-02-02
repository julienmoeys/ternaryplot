
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License       | 
# |             version 3                                  |
# +--------------------------------------------------------+



# ternary2SpatialPoints ====================================

#'Converts ternary*-class objects to SpatialPoints
#'
#'Converts ternary*-class objects to 
#'  \code{\link[sp]{SpatialPoints}}
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A single character string naming an existing  
#'      \code{\link[ternaryplot]{ternaryVariables-class}}.
#'  }  
#'  If \code{x} is \code{NULL}, them the vertices or the centroids 
#'  of the ternary classification in \code{s} will be retrieved 
#'  (see argument \code{what}).
#'
#'@param x 
#'  A \code{\link{data.frame}} containing the ternary data to 
#'  be converted to a \code{\link[sp]{SpatialPoints}}-object.
#'
#'@param what 
#'  Single character string. When \code{x} is missing, what 
#'  points should be extracted: the class vertices 
#'  (\code{what = 'vertices'}, the default) or the class 
#'  centroids (\code{what = 'centroids'})
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A \code{\link[sp]{SpatialPoints}}
#'
#'
#'@rdname ternary2SpatialPoints-methods
#'
#'@example inst/examples/ternary2SpatialPoints-example.R
#'
#'@export 
#'
ternary2SpatialPoints <- function(
 s, 
 ... 
){  
    UseMethod( "ternary2SpatialPoints" ) 
}   


    ## #@rdname ternary2SpatialPoints-methods
    ## #
    ## #@method ternary2SpatialPoints data.frame
    ## #
    ## #@export
    ## #
    ## #
    ## #@usage \method{ternary2SpatialPoints}{data.frame}( x, s, ... ) 
    ## #
    ## #@importFrom sp SpatialPoints 
    # ternary2SpatialPoints.data.frame <- function(
     # s, 
     # x, 
     # ... 
    # ){  
        # xy <- ternary2xy( s = s, x = x ) 
        
        # xy <- xy[, c( "x", "y" ) ] 
        
        # xy <- sp::SpatialPoints( coords = xy ) 
        
        # return( xy )  
    # }   


#'@rdname ternary2SpatialPoints-methods
#'
#'@method ternary2SpatialPoints ternarySystem
#'
#'@export
#'
#'@importFrom sp SpatialPoints 
#'@importFrom sp coordinates 
ternary2SpatialPoints.ternarySystem <- function(
 s, 
 x = NULL, 
 what = "vertices", 
 ... 
){  
    if( is.null( x ) ){
        if( what == "vertices" ){
            pts <- s[[ "vertices" ]]
            
            if( !is.null( pts ) ){
                xy <- ternary2xy( s = s, x = pts ) 
                
                xy <- xy[, c( "x", "y" ) ] 
                
                xy <- sp::SpatialPoints( coords = xy ) 
            }else{
                xy <- NULL 
            }   
            
        }else if( what == "centroids" ){
            pol <- ternary2SpatialPolygons( s = s ) 
            
            if( !is.null( pol ) ){
                xy <- sp::coordinates( pol ) 
                
                colnames( xy ) <- c( "x", "y" )
                
                xy <- sp::SpatialPoints( coords = xy ) 
            }else{
                xy <- NULL 
            }   
        }else{
            stop( sprintf( "Unknown / unsupported value for argument 'what' (%s)", what ) )
        }   
    }else{
        xy <- ternary2xy( s = s, x = x ) 
        
        xy <- xy[, c( "x", "y" ) ] 
        
        xy <- sp::SpatialPoints( coords = xy ) 
    }   
    
    return( xy )  
}   


#'@rdname ternary2SpatialPoints-methods
#'
#'@method ternary2SpatialPoints character
#'
#'@export
#'
ternary2SpatialPoints.character <- function(
 s, 
 what = "vertices", 
 ... 
){  
    s <- getTernarySystem( s = s ) 
    
    return( ternary2SpatialPoints.ternarySystem( s = s, what = what ) ) 
}   


