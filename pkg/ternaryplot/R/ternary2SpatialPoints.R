
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
#'@param x 
#'  Either \code{\link{data.frame}} containing the ternary data to be converted 
#'  to a \code{\link[sp]{SpatialPoints}}-object, or a \code{ternarySystem}, or a 
#'  single character string naming a ternary system to be fetched with 
#'  \code{\link[ternaryplot]{getTernarySystem}} from which the class vertices or 
#'  the class centroids should be extracted (see argument \code{what} below). 
#'
#'@param s 
#'  Only needed if \code{x} is a \code{\link{data.frame}}. 
#'  Either a \code{ternarySystem} object, such as obtained with 
#'  \code{\link[ternaryplot]{getTernarySystem}} or 
#'  \code{\link[ternaryplot]{createTernarySystem}}, or a character 
#'  string naming a \code{ternarySystem} to be fetched with 
#'  \code{\link[ternaryplot]{getTernarySystem}}. Define the \code{ternarySystem} 
#'  to be used to project the ternary data into an x-y system (as used by 
#'  \code{\link[sp]{sp}}).
#'
#'@param what 
#'  Single character string. When \code{x} is a \code{ternarySystem}, what points 
#'  should be extracted: the class vertices (\code{what = 'vertices'}, the default) 
#'  or the class centroids (\code{what = 'centroids'})
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
 x, 
 ... 
){  
    UseMethod( "ternary2SpatialPoints" ) 
}   


#'@rdname ternary2SpatialPoints-methods
#'
#'@method ternary2SpatialPoints data.frame
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPoints}{data.frame}( x, s, ... ) 
#'
#'@importFrom sp SpatialPoints 
ternary2SpatialPoints.data.frame <- function(
 x, 
 s, 
 ... 
){  
    xy <- ternary2xy( s = s, x = x ) 
    
    xy <- xy[, c( "x", "y" ) ] 
    
    xy <- sp::SpatialPoints( coords = xy ) 
    
    return( xy )  
}   


#'@rdname ternary2SpatialPoints-methods
#'
#'@method ternary2SpatialPoints ternarySystem
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPoints}{ternarySystem}( x, what = "vertices", ... ) 
#'
#'@importFrom sp SpatialPoints 
#'@importFrom sp coordinates 
ternary2SpatialPoints.ternarySystem <- function(
 x, 
 what = "vertices", 
 ... 
){  
    if( what == "vertices" ){
        pts <- x[[ "vertices" ]]
        
        if( !is.null( pts ) ){
            xy <- ternary2xy( s = x, x = pts ) 
            
            xy <- xy[, c( "x", "y" ) ] 
            
            xy <- sp::SpatialPoints( coords = xy ) 
        }else{
            xy <- NULL 
        }   
        
    }else if( what == "centroids" ){
        pol <- ternary2SpatialPolygons( x = x ) 
        
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
    
    return( xy )  
}   


#'@rdname ternary2SpatialPoints-methods
#'
#'@method ternary2SpatialPoints character
#'
#'@export
#'
#'
#'@usage \method{ternary2SpatialPoints}{character}( x, what = "vertices", ... ) 
#'
ternary2SpatialPoints.character <- function(
 x, 
 what = "vertices", 
 ... 
){  
    s <- getTernarySystem( s = x ) 
    
    return( ternary2SpatialPoints.ternarySystem( x = s, what = what ) ) 
}   


