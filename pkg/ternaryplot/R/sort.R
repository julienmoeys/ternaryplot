
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License       | 
# |             version 3                                  |
# +--------------------------------------------------------+



# sort.ternarySystem =======================================

#'Sort method for ternary classification (class centroids of ternarySystem).
#'
#'Sort method for ternary classification (class centroids of 
#'  a \code{\link[ternaryplot]{ternarySystem-class}}), 
#'  after two of its three ternary-variables (increasing or 
#'  decreasing order).
#'  
#'  The order in which the classes are defined matters when 
#'  classifying ternary data (in case a data point lies 
#'  between two or more classes). This function aims at 
#'  helping users that implements new classification 
#'  systems to check how these classes could be sorted (for 
#'  example in the case of a soil texture classification, 
#'  the classes could be defined by order of decreasing clay 
#'  and (then) sand content).
#'
#'
#'@param x 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A single character string naming an existing  
#'      \code{\link[ternaryplot]{ternaryVariables-class}}.
#'  }  
#'
#'@param decreasing 
#'  Single logical value. Should the ternary 
#'
#'@param by 
#'  A vector of 2 character strings. Names of the two ternary 
#'  variables (from \code{blrNames(x)}) after which the class 
#'  centroids should be sorted. 
#'
#'@param \dots 
#'  Additional parameters passed to \code{\link[base]{order}}.
#'
#'
#'@return 
#'  A \code{\link[base]{data.frame}}. The output of 
#'  \code{as.data.frame(x,what='centroids')} ordered ordered 
#'  after increasing (the default) or decreasing values of 
#'  the two ternary-variables in \code{by}. 
#'
#'
#'@rdname sort-methods
#'
#'@example inst/examples/sort-example.R
#'
#'@method sort ternarySystem
#'
#'@export
#'
sort.ternarySystem <- function(
    x, 
    decreasing = FALSE, 
    by, 
    ... 
){  
    if( missing( "by" ) ){
        stop( "'by' is missing with no default." )
    }   
    
    if( length( by ) != 2L ){
        stop( sprintf( 
            "'by' must be a vector of length 2 (length: %s)", 
            length( by ) 
        ) ) 
    }   
    
    centroids <- as.data.frame.ternarySystem( 
        x    = x, 
        what = 'centroids' ) 
    
    if( !is.null( centroids ) ){
        blrNames0 <- blrNames( s = x ) 
        
        if( any( colInX <- !(by %in% blrNames0) ) ){
            stop( sprintf(
                "Some variable(s) in 'by' not found in blrNames( x ): %s", 
                paste( by[ colInX ], collapse = "; " ) 
            ) ) 
        }   
        
        o <- order( 
            centroids[, by[ 1L ] ], 
            centroids[, by[ 2L ] ], 
            decreasing = decreasing, 
            ... )
        
        centroids <- centroids[ o, ]
    }   
    
    return( centroids )  
}   

