
# +--------------------------------------------------------+
# | Language: R + roxygen2 inline documentation            |
# | Package: soiltexture 2                                 |
# | Author(s): Julien Moeys <julienmoeys@@yahoo.fr>        |
# | License: AGPL3, Affero General Public License v.3      | 
# +--------------------------------------------------------+




# print.textureSystem ====================================== 

#'Print the content of a textureSystem object in a human readable format.
#'
#'Print the content of a 
#'  \code{\link[soiltexture2]{textureSystem-class}} object 
#'  (S3-class) in a human readable format.
#'
#'
#'@param x 
#'  A \code{\link[soiltexture2]{textureSystem-class}} object, 
#'  as created with \code{\link[soiltexture2]{createTextureSystem}}.
#'
#'@param prefix 
#'  Single character string. Prefix used before the different 
#'  items in \code{x} (intended for internal use, for example 
#'  \code{prefix = "$ternarySystem"}).
#'
#'@param collapse 
#'  Single character string. Passed to 
#'  \code{\link{paste}( ..., collapse )} when displaying the 
#'  items' values.
#'
#'@param \dots
#'  Additional parameters passed to specific methods (not 
#'  used).
#'
#'@export 
#'
#'@method print textureSystem
#' 
print.textureSystem <- function( 
    x, 
    prefix = "", 
    collapse = "; ", 
    ... 
){  
    cat( "A textureSystem (S3-class) object:\n\n" )
    
    cat( "$ternarySystem\n" ) 
    
    x.ts <- x
    class( x.ts ) <- "ternarySystem"
    
    print( 
        x        = x.ts, 
        prefix   = paste0( prefix, "$ternarySystem" ), 
        collapse = collapse, 
        ... 
    )   
    
    rm( x.ts )
    
    cat( "$particleSizeBoundaries\n" ) 
    
    print( x[[ "particleSizeBoundaries" ]] )
        
    return( invisible( x ) ) 
}   

