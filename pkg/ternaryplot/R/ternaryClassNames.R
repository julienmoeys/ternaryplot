
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+



# ternaryClassNames ========================================

#'Set or get the abbreviations (labels) of the classes of a ternary classification
#'
#'Set or get the abbreviations (labels) of the classes of a 
#'  ternary classification (a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} 
#'  object that contains a ternary classification).
#'
#'  Notice that this function should \emph{not} be used to 
#'  reorder the classes of a ternary classification. 
#'
#'  When changing/setting the abbreviations of a ternary 
#'  classification, \bold{it is very important to make sure 
#'  the new abbreviations}  (in \code{value}, see below) 
#'  \bold{are provided in the same order as the original 
#'  abbreviation of each class} (as returned by 
#'  \code{ternaryClassNames(s)}). No internal check is done 
#'  to make sure this is the case.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}.
#'
#'@param value 
#'  A vector of \code{n} character strings. New abbreviations 
#'  of the ternary classes in \code{s}. Class abbreviations 
#'  should be given in the same order as 
#'  output from \code{ternaryClassNames(s)}.
#'
#'
#'@return
#'  \code{ternaryClassNames(s)} returns a vector of character 
#'  strings, the class abbreviations, with as many values as 
#'  classes in \code{s}. If \code{s} does not have any classes 
#'  it returns an empty character string (\code{character(0)}). 
#'
#' 
#'@example inst/examples/ternaryClassNames-example.R
#' 
#'@rdname ternaryClassNames-methods
#'
#'@export 
#'
ternaryClassNames <- function( s, ... ){  
    UseMethod( "ternaryClassNames" ) 
}   



#'@rdname ternaryClassNames-methods
#'
#'@method ternaryClassNames ternarySystem
#'
#'@export
#'
ternaryClassNames.ternarySystem <- function( s, ... ){  
    out <- s[[ 'classes']]
    
    if( is.null( out ) ){
        out <- character(0)
    }else{
        out <- out[, "abbrev" ]
    }   
    
    return( out ) 
}   



# ternaryClassNames()<- ================================================== 

#'@rdname ternaryClassNames-methods
#'
#'@usage ternaryClassNames(s, ... ) <- value
#'
#'@export
#'
`ternaryClassNames<-` <- function( 
    s, 
    ..., 
    value 
){  
    UseMethod( "ternaryClassNames<-" ) 
}   



#'@rdname ternaryClassNames-methods
#'
#'@method ternaryClassNames<- ternarySystem
#'
#'@export
#'
#'@usage \method{ternaryClassNames}{ternarySystem}(s, ...) <- value
#'
`ternaryClassNames<-.ternarySystem` <- function( 
    s, 
    ..., 
    value 
){  
    oldValues <- ternaryClassNames.ternarySystem( s = s ) 
    
    if( !is.character( value ) ){
        stop( sprintf(
            "'value' must be a vector of character strings (now class(values) is %s)", 
            paste( class( value ), collapse = "; " ) 
        ) ) 
    }   
    
    if( length( value ) != length( oldValues ) ){
        stop( sprintf(
            "length(value) (%s) must be the same as length(ternaryClassNames(s)) (%s)", 
            length( value ), length( oldValues ) 
        ) ) 
    }   
    
    if( length( oldValues ) > 0 ){
        s[[ 'classes' ]][, 'abbrev' ] <- value 
        rownames( s[[ 'classes' ]] )  <- value 
    }   
    
    # names( value ) <- oldValues 
    
    return( s ) 
}   

