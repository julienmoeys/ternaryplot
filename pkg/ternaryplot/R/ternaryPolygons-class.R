
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+




# ternaryPolygons-class ====================================

#'Class to hold ternary polygons (ternary grids, ternary classification, ...)
#'
#'Class (\code{S3}) to hold ternary polygons (ternary grids, 
#'  ternary classification, ...). A 
#'  \code{ternaryPolygons}-class object is 
#'  a tagged \code{\link[base]{data.frame}} to which is 
#'  associated several \code{\link[base]{attributes}}. 
#'
#'  Each row in the \code{data.frame} contains the ternary 
#'  coordinates of one vertex of one polygon. Polygons should 
#'  of course have at least 3 vertices and there can be 
#'  as many polygons as needed.
#'
#'
#'@details 
#'  The \code{S3} \code{\link[base]{class}} system is an 
#'  \emph{in}formal and lightweight class system.
#'
#'
#'@section data.frame attributes:
#'
#'  Note: see \code{\link[base]{attr}} to retrieve or change 
#'  attributes.
#'  
#'  The \code{\link[base]{data.frame}} has the following 
#'  attributes:
#'
#'\itemize{
#'  \item \bold{\code{ternarySystem}}: A 
#'    \code{\link[ternaryplot]{ternarySystem-class}} object. 
#'    The ternary system associated with the ternary polygons.
#'  \item \bold{\code{idCol}}: A single character string. 
#'    name of the column in the \code{\link[base]{data.frame}} 
#'    that contains the identifiers of the different polygons.
#'  \item \bold{\code{labels}}: A vector of character strings, 
#'    the labels to be displayed inside the polygons when 
#'    plotting them on a ternary plot. There should be 
#'    one label per polygon in the \code{\link[base]{data.frame}}, 
#'    and the labels should be given in the same order as 
#'    the polygons identifiers in the 
#'    \code{\link[base]{data.frame}}.
#'  \item \bold{\code{data}}: An optional 
#'    \code{\link[base]{data.frame}} containing additional 
#'    variables for each of the ternary polygon. It should 
#'    have at least one column, \code{idCol} the identifiers 
#'    of the different polygons (see above and below).
#'}  
#'
#'
#'@section data.frame columns:
#'  
#'  The \code{\link[base]{data.frame}} has the following 
#'  column names:
#'  
#'\itemize{
#'  \item 3 columns named after the argument \code{blrNames} 
#'    in the \code{ternarySystem} attribute. Use 
#'    \code{\link[ternaryplot]{blrNames}( attr( x, "ternarySystem" ) )} 
#'    (where \code{x} is a \code{ternaryPolygons-class} object) 
#'    to fetch the current variable names.
#'  \item 1 column named after the attribute \code{idCol}. 
#'}  
#'
#'@seealso
#'\itemize{
#'  \item \code{\link[ternaryplot]{createTernaryGrid}} 
#'    to create a regular ternary grid (of triangular 
#'    polygons) and \code{\link[ternaryplot]{ternaryClasses}} 
#'    to extract the ternary classification of a ternary system 
#'    as \code{ternaryPolygons-class}. 
#'  \item \code{\link[ternaryplot]{ternaryPolygons}} (a 
#'    function, not to be confused with 
#'    \code{ternaryPolygons-class}, the class definition) to 
#'    plot ternary polygons on a ternary triangle.
#'} 
#'
#'
#'@name ternaryPolygons-class
#'
#'@rdname ternaryPolygons-class
#'
NULL 



# ternaryCheck.ternaryPolygons =============================

#'@rdname ternaryCheck-methods
#'
#'@method ternaryCheck ternaryPolygons
#'
#'@export 
#'
ternaryCheck.ternaryPolygons <- function(
 s, 
 ... 
){  
    valid <- TRUE 
    
    .tpPar <- tpPar()
    
    onFailure <- .tpPar[[ "onFailure" ]] # getTpPar( "onFailure" ) 
    
    
    #   Check the class of 's'
    if( !is.data.frame( s ) ){
        onFailure( sprintf( 
            "'s' should be a data.frame. Now a %s", 
            paste( class( s ), collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    }   
    
    
    #   Check that 's' has a ternarySystem attribute:
    s_ts <- attr( x = s, which = "ternarySystem" ) 
    
    if( is.null( s_ts ) ){
        onFailure( "attribute 'ternarySystem' in 's' is NULL (attr(s,'ternarySystem'))." )
        
        valid <- FALSE 
    }   
    
    if( !("ternarySystem" %in% class(s_ts)) ){
        onFailure( sprintf( 
            "attr(s,'ternarySystem') should be a ternarySystem (now a %s)", 
            paste( class( s_ts ), collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    }   
    
    
    #   Check the ternary System itself
    valid <- valid & ternaryCheck.ternarySystem( s = s_ts, ... )
    
    
    #   Check that the expected bottom left right variables 
    #   are present in 's'
    blrNames0 <- blrNames( s = s_ts ) 
    
    testBlrNames <- blrNames0 %in% colnames( s ) 
    
    if( !all( testBlrNames ) ){
        onFailure( sprintf( 
            "Some expected columns in 's' are missing: %s (ternary variables).", 
            paste( blrNames0[ !testBlrNames ], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    }   
    
    
    #   Check the idCol attribute
    idCol <- attr( x = s, which = "idCol" ) 
    
    if( is.null( idCol ) ){
        onFailure( "attribute 'idCol' in 's' is NULL (attr(s,'idCol'))." )
        
        valid <- FALSE 
    }   
    
    if( idCol %in% colnames( s ) ){
        onFailure( sprintf( 
            "Identifier column %s (attr(s,'idCol')) cannot be found in 's'", 
            idCol 
        ) ) 
        
        valid <- FALSE 
    }   
    
    
    #   Check the 'data' attribute, if present
    data <- attr( x = s, which = "data" ) 
    
    if( !is.null( data ) ){
        if( (!is.data.frame( data )) | (!is.matrix( data )) ){
            onFailure( sprintf( 
                "attr(s,'data') should be a data.frame or a matrix (now a %s)", 
                paste( class( data ), collapse = "; " ) 
            ) ) 
            
            valid <- FALSE 
        }   
        
        if( idCol %in% colnames( data ) ){
            onFailure( sprintf( 
                "Column %s ('idCol') cannot be found in attr(s,'data')", 
                idCol 
            ) ) 
            
            valid <- FALSE 
        }   
        
        if( !all( data[, idCol ] %in% s[, idCol ] ) ){
            onFailure( sprintf( 
                "Some values in attr(s,'data')[, %s ] cannot be found in s[, %s ] (%s is 'idCol')", 
                idCol, idCol, idCol
            ) ) 
            
            valid <- FALSE 
        }   
        
        if( any( duplicated( data[, idCol ] ) ) ){
            onFailure( sprintf( 
                "Some values in attr(s,'data')[, %s ] are duplicated, while they should be unique (%s is 'idCol')", 
                idCol, idCol  
            ) ) 
            
            valid <- FALSE 
        }   
    }   
    
    
    return( valid ) 
}   

