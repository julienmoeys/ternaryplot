
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License       | 
# |             version 3                                  |
# +--------------------------------------------------------+



# as.data.frame.ternarySystem ==============================

#'Extract the class vertices or the class centroids of a ternarySystem.
#'
#'Extract the class vertices or the class centroids of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param x 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object.
#'  Contrary to many other functions in the 
#'  ternaryplot-package, \code{x} should \emph{not} be a 
#'  character string naming an existing \code{ternarySystem} 
#'  (because the method 
#'  \code{\link[base]{as.data.frame.character}} 
#'  is already defined for other purposes).
#'
#'@param what 
#'  Single character string. What points should be extracted: 
#'  the class vertices (\code{what = 'vertices'}, the default) 
#'  or the class centroids (\code{what = 'centroids'})
#'
#'@param \dots 
#'  Additional parameters. Not used.
#'
#'
#'@return 
#'  A \code{\link[base]{data.frame}}. If \code{what} is 
#'  \code{'vertices'}, each row is a vertex, and the column 
#'  returned are \code{id}, as well as the 3 ternary variables 
#'  (as returned by \code{blrNames(s)}). If \code{what} is 
#'  \code{'centroids'}, the column 
#'  returned are \code{abbrev}, \code{name}, as well as the 
#'  3 ternary variables (as returned by \code{blrNames(s)}).
#'
#'
#'@rdname as.data.frame-methods
#'
#'@example inst/examples/as.data.frame-example.R
#'
#'@export 
#'
#'@importFrom sp SpatialPoints 
#'@importFrom sp coordinates 
as.data.frame.ternarySystem <- function(
    x, 
    what = 'vertices', 
    ... 
){  
    if( what == "vertices" ){
        pts <- x[[ "vertices" ]] 
        
    }else if( what == "centroids" ){
        pol <- ternary2SpatialPolygons( s = x ) 
        
        if( !is.null( pol ) ){
            xy <- sp::coordinates( pol ) 
            
            colnames( xy ) <- c( "x", "y" )
            
            pts <- xy2ternary( s = x, data = xy, 
                xyNames = c( "x", "y" ) ) 
            
            cls <- x[[ "classes" ]]  
            
            pts <- data.frame(
                cls[, c( "abbrev", "name" ) ], 
                pts, 
                stringsAsFactors = FALSE 
            )   
        }else{
            pts <- NULL 
        }   
    }else{
        stop( sprintf( "Unknown / unsupported value for argument 'what' (%s)", what ) )
    }   
    
    return( pts )
}   
