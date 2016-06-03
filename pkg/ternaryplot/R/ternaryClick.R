
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                   |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



#'Click on a ternary plot to get values of the ternary variables at that location. 
#'
#'Click on a ternary plot to get values of the ternary 
#'  variables at that location. 
#'
#'
#'@seealso \code{\link[graphics]{locator}} and 
#'  \code{\link[ternaryplot]{xy2ternary}}. The first is used 
#'  to retrieve the x-y coordinates from the ternary plot 
#'  and the later to convert them into ternary-variables.
#'
#'
#'@param s
#'  A \code{\link[ternaryplot]{ternarySystem}} object or a 
#'  character string naming an pre-defined 
#'  \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param n 
#'  Single integer value. Number of clicks on the ternary plot.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[graphics]{locator}}.
#'
#'
#'@return 
#'  Returns a \code{\link[base]{data.frame}} with ternary 
#'  point-data. With \code{n} rows and 3 columns, one per 
#'  ternary-variable (same names as \code{blrNames(s)}). 
#'
#'
#'@rdname ternaryClick-methods
#'
#'@examples inst/examples/ternaryClick-example.R
#'
#'@export 
#'
ternaryClick <- function( 
    s, 
    n = 1L, 
    ... 
){  
    if( missing( s ) ){ 
        UseMethod( "ternaryClick", object = character() )  
    }else{ 
        UseMethod( "ternaryClick" ) 
    }   
}   


#'@rdname ternaryClick-methods
#'
#'@method ternaryClick character
#'
#'@export
#'
ternaryClick.character <- function(
    s, 
    n = 1L, 
    ... 
){  
    if( missing( s ) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s ) 
    }   
    
    return( ternaryClick.ternarySystem( s = s, n = n, ... ) )
}   


#'@rdname ternaryClick-methods
#'
#'@method ternaryClick ternarySystem
#'
#'@export
#'
ternaryClick.ternarySystem <- function( 
    s, 
    n = 1L, 
    ... 
){      
    message( sprintf( 
        "Select %s points inside (or outside) the ternary plot", 
        n 
    ) ) 
    flush.console() 
    
    xyData <- as.data.frame( locator( n = n, ... ) ) 
    
    tData <- xy2ternary(
        s       = s, 
        data    = xyData, 
        xyNames = c( "x", "y" ) 
    )   
    
    return( tData ) 
}   

