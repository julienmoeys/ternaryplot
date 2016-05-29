
# +--------------------------------------------------------+
# | Language: R + roxygen2 inline documentation            |
# | Package: soiltexture 2                                 |
# | Author(s): Julien Moeys <julienmoeys@@yahoo.fr>        |
# | License: AGPL3, Affero General Public License v.3      | 
# +--------------------------------------------------------+



# ternaryCheck =============================================

#'@export ternaryCheck.textureSystem 
NULL 

#'Check the validity of ternary*-class objects and ternary data.
#'
#'Check the validity of ternary*-class objects and ternary data.
#'
#'
#'@seealso Arguments \code{onFailure} and \code{okClock} in 
#'  \code{\link[ternaryplot]{getTpPar}} (ternaryplot-package 
#'  options). \code{\link[ternaryplot]{ternaryCheck}} help 
#'  page in the ternaryplot-package for the generic function. 
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item a \code{\link[soiltexture2]{textureSystem-class}} 
#'      (needed when \code{x} is not \code{NULL}), or 
#'    \item a \code{\link[ternaryplot]{ternarySystem-class}} 
#'      (needed when \code{x} is not \code{NULL}), or 
#'    \item a \code{\link[ternaryplot]{ternaryVariables-class}}, or 
#'    \item a \code{\link[ternaryplot]{ternaryGeometry-class}} 
#'      object. 
#'  }  
#'  Object whose validity is to be evaluated. When \code{x} 
#'  is not \code{NULL}, it is the validity of the data in 
#'  \code{x} that is checked, not that of \code{s}.
#'
#'@param x 
#'  If not \code{NULL}, a \code{\link[base]{data.frame}} or 
#'  a \code{\link[base]{matrix}} containing ternary data to 
#'  be checked.
#'
#'@param testRange 
#'  Single logical. Test if the range of fraction is between 
#'  0 and the expected sum of fractions (1 or 100\%). Only 
#'  when 
#'
#'@param testSum 
#'  Single logical. Test if the sum of the 3 fractions is 
#'  equal to the expected sum of fractions (1 or 100\%).
#'
#'@param \dots
#'  Not used.
#'
#'
#'@return
#'  Whether an output is returned depends on the package 
#'  parameter \code{onFailure} (see 
#'  \code{\link[ternaryplot]{tpPar}}; the function to be 
#'  used when a problem is found). If \code{onFailure} is 
#'  \code{\link[base]{stop}}, then an error is raised and 
#'  the evaluation stops. If it is a 
#'  \code{\link[base]{warning}} or a 
#'  \code{\link[base]{message}} the evaluation continues 
#'  and \code{TRUE} is returned when no problem was found, 
#'  and \code{FALSE} when one or several problems were found.
#'
#'
#'@rdname ternaryCheck-methods
#'
#'@method ternaryCheck textureSystem
#'
ternaryCheck.textureSystem <- function(
    s, 
    x = NULL, 
    testRange, 
    testSum, 
    ... 
){  
    # s0 <- s
    # class( s0 ) <- class( s0 )[ class( s0 ) != "textureSystem" ]
    valid <- ternaryCheck.ternarySystem( s = s, x = x, 
        testRange = testRange, testSum = testSum, ... )
    # rm( s0 )
    
    onFailure <- getTpPar( "onFailure" )  
    
    # Check the validity of the "particleSizeBoundaries" item
    # ------------------------------------------------------
    
    ## Does the item exists
    if( is.null( x ) ){
        if( !("particleSizeBoundaries" %in% names(s)) ){
            onFailure( "Some items (or item-labels) are missing in 's': 'particleSizeBoundaries'" )
            
            valid <- FALSE 
        }   
    }   
    
    
    ## Does it has the right class
    if( !any(c( "numeric", "integer" ) %in% class( s[[ "particleSizeBoundaries" ]] )) ){
        onFailure( "'particleSizeBoundaries' should be a 'numeric' or 'integer' vector" ) 
        
        valid <- FALSE 
    }   
    
    
    ## Does it has the right lenght?
    l <- length( s[[ "particleSizeBoundaries" ]] ) 
    if( l != 4 ){
        onFailure( "'particleSizeBoundaries' should be a vector of length 4 (now %s)", l )
        
        valid <- FALSE 
    };  rm( l )
    
    
    ## Does it has the right tags?
    nm <- names( s[[ "particleSizeBoundaries" ]] ) 
    if( is.null( nm ) ){
        onFailure( "names( s[['particleSizeBoundaries']] ) is NULL. 's' must be a tagged vector." ) 
        
        valid <- FALSE 
    }   
    
    .blrNames <- blrNames( s ) 
    testNames <- .blrNames %in% nm 
    if( !all( testNames ) ){
        onFailure( 
            "some tags in names( s[['particleSizeBoundaries']] ) are missing: %s", 
            paste( .blrNames[ !testNames ], sep = "; " ) 
        )    
        
        valid <- FALSE 
    };  rm( testNames )
    
    testNames2 <- nm[ 1L ] %in% .blrNames  
    if( testNames2 ){
        onFailure( 
            "names( s[['particleSizeBoundaries']] )[ 1 ] (the 1st tag) should not be one of %s (any other value is fine)", 
            paste( .blrNames, sep = ", " ) 
        )    
        
        valid <- FALSE 
    };  rm( testNames2, nm, .blrNames )
    
    
    ## Does it has ordered values?
    testOrder <- diff( as.numeric( s[[ "particleSizeBoundaries" ]] ) ) 
    testOrder <- testOrder > 0 
    if( !all( testOrder ) ){
        onFailure( "Values in s[['particleSizeBoundaries']] should be ordered and increasing." )    
        
        valid <- FALSE 
    };  rm( testOrder )
    
    
    return( valid ) 
}   




# textureSystem-class ======================================

#'Class to hold the definition of a textureSystem (the full definition of texture plot and optionally classification).
#'
#'Class (\code{S3}) to hold the definition of a textureSystem 
#'  (the full definition of texture plot and optionally 
#'  classification). A \code{textureSystem-class} object is 
#'  a tagged \code{\link[base]{list}} of several 
#'  objects/items (see below). A 'tagged' list means a list of 
#'  the form \code{list( "tag1" = value1, "tag2" = value2, 
#'  ... )}.
#'
#'
#'@details 
#'  The \code{S3} \code{\link[base]{class}} system is an 
#'  \emph{in}formal and lightweight class system.
#'
#'
#'@section List objects/items:
#'
#'  A \code{textureSystem-class} object has all the features 
#'  (items) of a A \code{\link[ternaryplot]{ternarySystem-class}} 
#'  object, plus the following item(s): 
#'
#'\itemize{
#'  \item \bold{\code{particleSizeBoundaries}}: A tagged vector 
#'    of numeric or integer values giving the boundaries of 
#'    the particle size classes. The vector must be ordered 
#'    and increasing. The length of the vector must be 4, 
#'    with the 1st value giving the (absolute) lowest value 
#'    of the particle size, and the 3 remaining values the 
#'    upper boundary of each texture particle size class.
#'    The tags of the 1st item can be any value (even 
#'    \code{""}) and the 3 remaining tags \bold{must} 
#'    correspond to the variable-name of the 3 texture 
#'    particle size classes (as can be obtained with 
#'    \code{blrNames(s)}, where \code{s} is the 
#'    \code{textureSystem-class} object, but the order of does 
#'    not has to be the same of course). Finally, the 
#'    \bold{unit} should (preferably) be in micrometers. 
#'    
#'    A valid example could be \code{c(0,"CLAY"=2,"SILT"=50,"SAND"=2000)}.
#'} 
#'
#'
#'@seealso
#'
#'  A \code{textureSystem-class} object is compatible with 
#'  all the functions available for 
#'  \code{\link[ternaryplot]{ternarySystem-class}} (see 'See 
#'  also' in that help page). In addition to that, the following 
#'  functions can be relevant:
#'
#'\itemize{
#'  \item TO BE COMPLETED.
#'} 
#'
#'
#'@name textureSystem-class
#'
#'@rdname textureSystem-class
#'
NULL 




# createTextureSystem ======================================

#'Creates a textureSystem-class object (the full definition of texture plot and its optionally classification).
#'
#'Creates a \code{\link[soiltexture2]{textureSystem-class}} 
#'  object (the full definition of texture plot and its 
#'  optionally classification).
#'
#'
#'@param ternarySystem
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param particleSizeBoundaries
#'  See \code{\link[soiltexture2]{textureSystem-class}}.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}
#'
#'
#'@return 
#'  A \code{\link[soiltexture2]{textureSystem-class}}.
#'
#'
#'@seealso \code{\link[soiltexture2]{textureSystem-class}}.
#'
#'
#'@example inst/examples/createTextureSystem-example.R
#'
#'@rdname createTextureSystem
#'
#'@export
#'
createTextureSystem <- function(
    ternarySystem, 
    particleSizeBoundaries, 
    ...
){  
    if( missing( "ternarySystem" ) ){ 
        stop( "'ternarySystem' is missing. No default." ) 
    }   
    
    if( missing( "particleSizeBoundaries" ) ){ 
        stop( "'particleSizeBoundaries' is missing. No default." ) 
    }   
    
    
    ternarySystem[[ "particleSizeBoundaries" ]] <- particleSizeBoundaries 
    
    
    #   Set the class
    class( ternarySystem ) <- c( "textureSystem", "ternarySystem" )
    
    
    #   Check:
    ternaryCheck.textureSystem( s = ternarySystem, ... ) 
    
    
    return( ternarySystem ) 
}   

