
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+



# ternaryNormalise =========================================

#'Normalise the sum of ternary fractions in ternary data to 1 or 100\%.
#'
#'Normalise the sum of ternary fractions in ternary data to 
#'  1 [-] or 100\% (depending on 
#'  \code{\link[ternaryplot]{fracSum}}\code{(s)}), and optionally 
#'  returns the residual of the normalisation (as an extra 
#'  column added to \code{x}).
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}}, or 
#'    \item A character string naming a pre-defined ternary 
#'      \code{\link[ternaryplot]{ternarySystem-class}}.
#'  } 
#'  
#'@param x 
#'  A \code{\link[base]{data.frame}} containing the 
#'  ternary data to be normalised. It must contain 
#'  the 3 column names found in 
#'  \code{\link[ternaryplot]{blrNames}}\code{(s)}).
#'  
#'@param residuals 
#'  Single logical value. If \code{TRUE}, the residuals 
#'  of the normalisation are added to \code{x} (new column 
#'  named \code{residual}).
#'  
#'@param \dots 
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}.
#'
#'
#'@return
#'  A \code{\link[base]{data.frame}} with the normalised 
#'  ternary data. If \code{residuals} is \code{TRUE}, a new 
#'  column (\code{"residuals"}) is added to the 
#'  \code{data.frame} with the residuals of the normalisation: 
#'  Sum of ternary fractions after normalisation minus 
#'  sum of ternary fractions before normalisation. So if a 
#'  value in \code{"residuals"} is \code{> 0} it means the 
#'  sum of ternary fractions was below 1 or 100\%, while 
#'  if a value in \code{"residuals"} is \code{< 0} it means 
#'  the sum of ternary fractions was above 1 or 100\%.
#'
#'
#'@example inst/examples/ternaryNormalise-example.R
#'
#'@rdname ternaryNormalise-methods
#'
#'@export
#'
ternaryNormalise <- function( 
    s, 
    x, 
    ... 
){  
    UseMethod( "ternaryNormalise" ) 
}   


#'@rdname ternaryNormalise-methods
#'
#'@method ternaryNormalise ternarySystem
#'
#'@export
#'
ternaryNormalise.ternarySystem <- function( 
    s, 
    x, 
    residuals = TRUE, 
    ... 
){  
    #   Check the validity of the ternary data, 
    #   but not the sum of the 3 fractions
    ternaryCheck( x = x, s = s, testSum = FALSE, ... ) 
    
    #   Fetch the bottom left and right variable names
    blrNames0 <- blrNames( s = s ) 
    
    #   Fetch the sum of 3 fractions
    fracSum0 <- fracSum( s = s ) 
    
    #   Normalise
    rs              <- rowSums( x[, blrNames0 ] ) 
    x[, blrNames0 ] <- (x[, blrNames0 ] / rs) * fracSum0 
    
    #   Add residuals
    if( residuals ){
        rs2 <- rowSums( x[, blrNames0 ] ) 
        
        if( "residuals" %in% colnames(x) ){
            warning( "'x' already has a column 'residuals'. Residuals will not be output." )
        }else{
            x[, "residuals" ] <- rs2 - rs 
        }   
    }   
    
    #   Check the validity of the ternary data, 
    #   but not the sum of the 3 fractions
    ternaryCheck( x = x, s = s, ... ) 
    
    #   Return the normalised data
    return( x ) 
}   


#'@rdname ternaryNormalise-methods
#'
#'@method ternaryNormalise character
#'
#'@export
#'
ternaryNormalise.character <- function( 
    s, 
    x, 
    residuals = TRUE, 
    ... 
){  
    s <- getTernarySystem( s = s ) 
    
    #   Return the normalised data
    return( ternaryNormalise.ternarySystem( s = s, x = x, residuals = residuals, ... ) ) 
}   


