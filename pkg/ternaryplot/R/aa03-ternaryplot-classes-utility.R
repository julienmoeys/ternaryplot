
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+



# blrNames =================================================

#'Set or get the bottom-left-right axis names of a ternarySystem 
#'  object 
#'
#'Set or get the bottom-left-right axis names of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'  When setting the names, the function can be used to rename 
#'  the axis-variables, i.e. the same variables but a different 
#'  name, or to reorder them, i.e. the same variables and 
#'  the same names but in a different order (see \code{reorder} 
#'  below).
#'  
#'  When setting the names and \code{reorder} is \code{TRUE}, 
#'  the value of \code{blrLabels} is also reordered, so that 
#'  the two parameters are still coherent.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A \code{\link[ternaryplot]{ternaryVariables-class}} 
#'      object.
#'  }  
#'
#'@param reorder
#'  Single logical value. Only when setting new names. If 
#'  \code{FALSE} (the default), the name of the ternary 
#'  variables in the definition of the ternary classes is 
#'  also changed (if present). If \code{TRUE}, the name of the 
#'  ternary variables in the definition of the ternary classes 
#'  is preserved (if present). So use \code{FALSE} to rename 
#'  the variables but keep them on the same axis and use 
#'  \code{TRUE} to reorder them (i.e. the 
#'  same names but in a different order, on different axis). 
#'  The latter implies that the \code{blrNames(s)} and 
#'  \code{value} are the same but in a different order.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}.
#'
#'
#'@param value 
#'  A vector of three character strings. Names of the 
#'  bottom-left-right axis.
#'
#'
#'@return
#'  \code{blrNames(s)} returns a vector of three character 
#'  strings, the names of the bottom, left and right variables.
#'
#' 
#'@example inst/examples/blrNames-example.R
#' 
#'@rdname blrNames-methods
#'
#'@export 
#'
blrNames <- function( s, ... ){  
    UseMethod( "blrNames" ) 
}   



#'@rdname blrNames-methods
#'
#'@method blrNames ternarySystem
#'
#'@export
#'
blrNames.ternarySystem <- function( s, ... ){  
    return( s[[ 'ternaryVariables']][[ 'blrNames' ]] ) 
}   



#'@rdname blrNames-methods
#'
#'@method blrNames ternaryVariables
#'
#'@export
#'
blrNames.ternaryVariables <- function( s, ... ){  
    return( s[[ 'blrNames' ]] ) 
}   



# blrNames()<- ================================================== 

#'@rdname blrNames-methods
#'
#'@usage blrNames(s, reorder, ... ) <- value
#'
#'@export
#'
`blrNames<-` <- function( 
    s, 
    reorder = FALSE, 
    ..., 
    value 
){  
    UseMethod( "blrNames<-" ) 
}   



#'@rdname blrNames-methods
#'
#'@method blrNames<- ternarySystem
#'
#'@export
#'
#'@usage \method{blrNames}{ternarySystem}(s, reorder, ...) <- value
#'
`blrNames<-.ternarySystem` <- function( 
 s, 
 reorder = FALSE, 
 ..., 
 value 
){  
    #   Fetch the old names
    oldNames <- s[[ 'ternaryVariables' ]][[ 'blrNames' ]] 
    
    if( reorder ){
        test <- all( oldNames %in% value ) & 
            all( value %in% oldNames )
        
        if( !test ){
            stop( "When 'reorder' is TRUE, all the values in blrNames(s) must be in 'value' and the other way-round." ) 
        };  rm( test )
    }   
    
    s[[ 'ternaryVariables']][[ 'blrNames' ]] <- value 
    
    # Change the column names in vertices and the scale
    if( !reorder ){
        vertices <- s[[ 'vertices' ]] 
        scale    <- s[[ 'scale' ]] 
        
        verticeColWasChanged <- rep( FALSE, ncol( vertices ) ) 
        scaleColWasChanged   <- rep( FALSE, ncol( scale ) ) 
        
        for( o in 1:length( oldNames ) ){
            selVerticeCol <- (colnames( vertices ) == oldNames[ o ]) & 
                (!verticeColWasChanged) 
            colnames( vertices )[ selVerticeCol ] <- value[ o ] 
            verticeColWasChanged[ selVerticeCol ] <- TRUE 
            
            selScaleCol <- (colnames( scale ) == oldNames[ o ]) & 
                (!scaleColWasChanged) 
            colnames( scale )[ selScaleCol ] <- value[ o ] 
            scaleColWasChanged[ selScaleCol ] <- TRUE 
        }    
        
        rm( verticeColWasChanged, scaleColWasChanged, selVerticeCol, 
            selScaleCol )
        
        s[[ 'vertices' ]] <- vertices 
        s[[ 'scale' ]]    <- scale 
    }else{
        newOrder <- 1:length( oldNames )
        names( newOrder ) <- oldNames
        newOrder <- as.integer( newOrder[ value ] )
        
        #   Set the axis-labels
        s[[ "ternaryVariables" ]][[ "blrLabels" ]] <- 
            s[[ "ternaryVariables" ]][[ "blrLabels" ]][ newOrder ] 
    }   
    
    ternaryCheck.ternarySystem( s = s, ... )
    
    return( s ) 
}   



#'@rdname blrNames-methods
#'
#'@method blrNames<- ternaryVariables
#'
#'@export
#'
#'
#'@usage \method{blrNames}{ternaryVariables}(s, ...) <- value
#'
`blrNames<-.ternaryVariables` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'blrNames' ]] <- value 
        
    ternaryCheck.ternaryVariables( s = s, ... )
    
    return( s ) 
}   



# blrLabels ====================================================== 

#'Set or get the bottom-left-right axis labels of a ternarySystem 
#'  object 
#'
#'Set or get the bottom-left-right axis labels of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A \code{\link[ternaryplot]{ternaryVariables-class}} 
#'      object.
#'  }  
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[ternaryplot]{ternaryCheck}}.
#'
#'@param value 
#'  A vector of 3 character strings. Names of the 
#'  bottom-left-right axis.
#'
#'
#'@return
#'  \code{blrLabels(s)} returns a vector of three character 
#'  strings, the names of the bottom, left and right 
#'  variable-labels.
#'
#' 
#'@example inst/examples/blrLabels-example.R
#'
#'@rdname blrLabels-methods
#'
#'@export 
#'
blrLabels <- function( s, ... ){  
    UseMethod( "blrLabels" ) 
}   



#'@rdname blrLabels-methods
#'
#'@method blrLabels ternarySystem
#'
#'@export
#'
blrLabels.ternarySystem <- function( s, ... ){  
    return( s[[ 'ternaryVariables']][[ 'blrLabels' ]] ) 
}   



#'@rdname blrLabels-methods
#'
#'@method blrLabels ternaryVariables
#'
#'@export
#'
blrLabels.ternaryVariables <- function( s, ... ){  
    return( s[[ 'blrLabels' ]] ) 
}   



# blrLabels()<- ================================================== 

#'@rdname blrLabels-methods
#'
#'@usage blrLabels( s, ... ) <- value
#'
#'@export
#'
`blrLabels<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "blrLabels<-" ) 
}   



#'@rdname blrLabels-methods
#'
#'@method blrLabels<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{blrLabels}{ternarySystem}(s, ...) <- value
#'
`blrLabels<-.ternarySystem` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'ternaryVariables']][[ 'blrLabels' ]] <- value 
    
    ternaryCheck.ternarySystem( s = s, ... )
    
    return( s ) 
}   



#'@rdname blrLabels-methods
#'
#'@method blrLabels<- ternaryVariables
#'
#'@export
#'
#'
#'@usage \method{blrLabels}{ternaryVariables}(s, ...) <- value
#'
`blrLabels<-.ternaryVariables` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'blrLabels' ]] <- value 
    
    ternaryCheck.ternaryVariables( s = s, ... )
    
    return( s ) 
}   



# blrClock =================================================

#'Set or get the bottom-left-right orientation of a ternarySystem 
#'  object
#'
#'Set or get the bottom-left-right orientation of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A \code{\link[ternaryplot]{ternaryGeometry-class}} 
#'      object.
#'  }  
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}.
#'
#'@param value 
#'  A vector of 3 logical values. Bottom-left-right orientation of 
#'  a ternarySystem object.
#'
#'
#'@return
#'  \code{blrClock(s)} returns a vector of three logical values, 
#'  the direction of the bottom, left and right axis.
#'
#'
#'@example inst/examples/blrClock-example.R
#'
#'@rdname blrClock-methods
#'
#'@export 
#'
blrClock <- function( 
 s, 
 ... 
){  
    UseMethod( "blrClock" ) 
}   



#'@rdname blrClock-methods
#'
#'@method blrClock ternarySystem
#'
#'@export
#'
blrClock.ternarySystem <- function( 
 s, 
 ... 
){  
    return( s[[ 'ternaryGeometry' ]][[ 'blrClock' ]] ) 
}   



#'@rdname blrClock-methods
#'
#'@method blrClock ternaryGeometry
#'
#'@export
#'
blrClock.ternaryGeometry <- function( 
 s, 
 ... 
){  
    return( s[[ 'blrClock' ]] ) 
}   



# blrClock()<- ================================================== 

#'@rdname blrClock-methods
#'
#'@usage blrClock( s, ... ) <- value
#'
#'@export
#'
`blrClock<-` <- function( 
 s, 
 ...,
 value 
){  
    UseMethod( "blrClock<-" ) 
}   



#'@rdname blrClock-methods
#'
#'@method blrClock<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{blrClock}{ternarySystem}(s, ...) <- value
#'
`blrClock<-.ternarySystem` <- function( 
 s, 
 ...,
 value 
){  
    s[[ 'ternaryGeometry' ]][[ 'blrClock' ]] <- value 
    
    
    # #   Set the class (in case it has changed)
    # class( s[[ 'ternaryGeometry' ]] ) <- 
        # .generateTernaryGeometry2ndClass( blrClock = value ) 
    
    
    # #   Set the class (in case it has changed)
    # class( s ) <- .generateTernaryGeometry2ndClass( 
        # blrClock = value, 
        # class1   = "ternarySystem" ) 
    
    
    #   Check the validity
    ternaryCheck.ternaryGeometry( s = s[[ 'ternaryGeometry' ]], ... ) 

    
    return( s ) 
}   



#'@rdname blrClock-methods
#'
#'@method blrClock<- ternaryGeometry
#'
#'@export
#'
#'
#'@usage \method{blrClock}{ternaryGeometry}(s, ...) <- value
#'
`blrClock<-.ternaryGeometry` <- function( 
 s, 
 ...,
 value 
){  
    s[[ 'blrClock' ]] <- value 
    
    
    # #   Set the class (in case it has changed)
    # class( s ) <- .generateTernaryGeometry2ndClass( blrClock = value ) 
    
    
    ternaryCheck.ternaryGeometry( s = s, ... ) 
    
    return( s ) 
}   



# fracSum ======================================================= 

#'Set or get the sum of the three fractions of a ternarySystem 
#'  object
#'
#'Set or get the sum of the three fractions of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}} 
#'      object, or 
#'    \item A \code{\link[ternaryplot]{ternaryGeometry-class}} 
#'      object.
#'  }  
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@param value
#'  Single numerical value. Sum of the three fractions of the 
#'  ternarySystem.
#'
#'
#'@return
#'  \code{fracSum(s)} returns a single numeric value, the sum 
#'  of the three variables (1 if they are fractions and 100 
#'  if they are percentages).
#'
#' 
#'@rdname fracSum-methods
#'
#'@export 
#'
fracSum <- function( 
 s, 
 ... 
){  
    UseMethod( "fracSum" ) 
}   



#'@rdname fracSum-methods
#'
#'@method fracSum ternarySystem
#'
#'@export
#'
fracSum.ternarySystem <- function( 
 s, 
 ... 
){  
    return( s[[ 'ternaryGeometry' ]][[ 'fracSum' ]] ) 
}   
 


#'@rdname fracSum-methods
#'
#'@method fracSum ternaryGeometry
#'
#'@export
#'
fracSum.ternaryGeometry <- function( 
 s, 
 ... 
){  
    return( s[[ 'fracSum' ]] ) 
}   



# fracSum()<- =================================================== 

#'@rdname fracSum-methods
#'
#'@usage fracSum(s, ... ) <- value
#'
#'@export
#'
`fracSum<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "fracSum<-" ) 
}   



#'@rdname fracSum-methods
#'
#'@method fracSum<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{fracSum}{ternarySystem}(s, ...) <- value
#'
`fracSum<-.ternarySystem` <- function( 
 s, 
 ..., 
 value 
){ 
    s[[ 'ternaryGeometry' ]][[ 'fracSum' ]] <- value 
    
    ternaryCheck.ternaryGeometry( s = s[[ 'ternaryGeometry' ]], ... ) 
    
    return( s ) 
}   



#'@rdname fracSum-methods
#'
#'@method fracSum<- ternaryGeometry
#'
#'@export
#'
#'
#'@usage \method{fracSum}{ternaryGeometry}(s, ...) <- value
#'
`fracSum<-.ternaryGeometry` <- function( 
 s, 
 ..., 
 value 
){ 
    s[[ 'fracSum' ]] <- value 
    
    ternaryCheck.ternaryGeometry( s = s, ... ) 
    
    return( s ) 
}   



# tlrAngles ===================================================== 

#'Set or get the top, left and right angles of a ternary system 
#'  object
#'
#'Set or get the top, left and right angles of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, 
#'  as created with \code{\link[ternaryplot]{createTernarySystem}}, 
#'  or a \code{ternaryGeometry} object, as created with 
#'  \code{\link[ternaryplot]{createTernaryGeometry}}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@param value
#'  Vector of three numerical values, summing to 180. Top, left 
#'  and right angles of the ternary system object.
#'
#'
#'@return
#'  \code{tlrAngles(s)} returns a vector of three numeric 
#'  values, the angles of the top, left and right triangle 
#'  vertices (in degrees).
#'
#' 
#'@example inst/examples/tlrAngles-example.R
#'
#'@rdname tlrAngles-methods
#'
#'@export 
#'
tlrAngles <- function( 
 s, 
 ... 
){  
    UseMethod( "tlrAngles" ) 
}   



#'@rdname tlrAngles-methods
#'
#'@method tlrAngles ternarySystem
#'
#'@export
#'
tlrAngles.ternarySystem <- function( 
 s, 
 ... 
){  
    return( s[[ 'ternaryGeometry' ]][[ 'tlrAngles' ]] ) 
}   



#'@rdname tlrAngles-methods
#'
#'@method tlrAngles ternaryGeometry
#'
#'@export
#'
tlrAngles.ternaryGeometry <- function( 
 s, 
 ... 
){  
    return( s[[ 'tlrAngles' ]] ) 
}   



# tlrAngles()<- ================================================= 

#'@rdname tlrAngles-methods
#'
#'@usage tlrAngles( s, ... ) <- value 
#'
#'@export
#'
`tlrAngles<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "tlrAngles<-" ) 
}   
 


#'@rdname tlrAngles-methods
#'
#'@method tlrAngles<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{tlrAngles}{ternarySystem}(s, ...) <- value
#'
`tlrAngles<-.ternarySystem` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'ternaryGeometry' ]][[ 'tlrAngles' ]] <- value 
    
    ternaryCheck.ternaryGeometry( s = s[[ 'ternaryGeometry' ]], ... )     
    
    return( s ) 
}   



#'@rdname tlrAngles-methods
#'
#'@method tlrAngles<- ternaryGeometry
#'
#'@export
#'
#'
#'@usage \method{tlrAngles}{ternaryGeometry}(s, ...) <- value
#'
`tlrAngles<-.ternaryGeometry` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'tlrAngles' ]] <- value 
    
    ternaryCheck.ternaryGeometry( s = s, ... )     
    
    return( s ) 
}   



# ternaryGeometry ===================================================== 

#'Set or get the ternaryGeometry of a ternarySystem object
#'
#'Set or get the ternaryGeometry of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, 
#'  as created with 
#'  \code{\link[ternaryplot]{createTernarySystem}}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@param value
#'  A \code{ternaryGeometry} object, as created with 
#'  \code{\link[ternaryplot]{createTernaryGeometry}}.
#'
#'
#'@return
#'  \code{ternaryGeometry(s)} returns a 
#'  \code{\link[ternaryplot]{ternaryGeometry-class}} object.
#'
#'
#'@rdname ternaryGeometry-methods
#'
#'@export 
#'
ternaryGeometry <- function( 
 s, 
 ... 
){  
    UseMethod( "ternaryGeometry" ) 
}   



#'@rdname ternaryGeometry-methods
#'
#'@method ternaryGeometry ternarySystem
#'
#'@export
#'
ternaryGeometry.ternarySystem <- function( 
 s, 
 ... 
){  
    return( s[[ 'ternaryGeometry' ]] ) 
}   



# ternaryGeometry()<- =========================================== 

#'@rdname tlrAngles-methods
#'
#'@usage ternaryGeometry( s, ... ) <- value
#'
#'@export
#'
`ternaryGeometry<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "ternaryGeometry<-" ) 
}   
 


#'@rdname ternaryGeometry-methods
#'
#'@method ternaryGeometry<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{ternaryGeometry}{ternarySystem}( s, ... ) <- value
#'
`ternaryGeometry<-.ternarySystem` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'ternaryGeometry' ]] <- value 
    
    # #   Also set the class (esp the 2nd class)
    # #   Set the class
    # class( s[[ 'ternaryGeometry' ]] ) <- 
        # .generateTernaryGeometry2ndClass( 
            # blrClock = blrClock( s ), 
            # class1   = "ternaryGeometry" ) 
    
    # class( s ) <- 
        # .generateTernaryGeometry2ndClass( 
            # blrClock = blrClock( s ), 
            # class1   = "ternarySystem" ) 
    
    ternaryCheck.ternaryGeometry( s = s[[ 'ternaryGeometry' ]], ... )     
    
    return( s ) 
}   



# print.ternaryGeometry ========================================= 

#'Print the content of a ternaryGeometry object in a human readable format.
#'
#'Print the content of a \code{ternaryGeometry} object 
#'  (S3-class) in a human readable format.
#'
#'
#'@param x 
#'  A \code{ternaryGeometry} object, as created with 
#'  \code{\link[ternaryplot]{createTernaryGeometry}}.
#'
#'@param prefix 
#'  Single character string. Prefix used before the different 
#'  items in \code{x} (intended for internal use, for example 
#'  \code{prefix = "$ternaryGeometry"}).
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
#'
#'@method print ternaryGeometry
#'
#'@export
#'
print.ternaryGeometry <- function( 
 x, 
 prefix = "", 
 collapse = "; ", 
 ... 
){  
    cat( "A ternaryGeometry (S3-class) object:\n\n" )
    
    cat( sprintf( 
        "%s$tlrAngles: %s\n", 
        prefix, 
        paste( as.character( x[[ "tlrAngles" ]] ), collapse = collapse ) 
    ) ) 
    
    cat( "  Angles of the top, left and right vertices [degrees]\n" )
    cat( "  Get or set with tlrAngles() or tlrAngles() <- value\n\n" )
    
    clock <- as.character( x[[ "blrClock" ]] )
    clock[ is.na( clock ) ] <- "NA" 
    
    cat( sprintf( 
        "%s$blrClock: %s\n", 
        prefix, 
        paste( clock, collapse = collapse ) 
    ) ) 
    
    cat( "  Directions of the bottom, left and right axis (edges)\n" )
    cat( "  TRUE is clockwise (CW), FALSE is counter-CW and NA is centripetal\n" )
    cat( "  Get or set with blrClock() or blrClock() <- value\n\n" )
    
    cat( sprintf( 
        "%s$fracSum: %s\n", 
        prefix, 
        x[[ "fracSum" ]] 
    ) ) 
    
    cat( "  Sum of the 3 variables in the diagram\n" )
    cat( "  1 for fractions, 100 for percentages\n" )
    cat( "  Get or set with fracSum() or fracSum() <- value\n\n" )
    
    return( invisible( x ) ) 
}   



# ternaryVariables ============================================== 

#'Set or get the ternaryVariables of a ternarySystem object.
#'
#'Set or get the ternaryVariables of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, 
#'  as created with \code{\link[ternaryplot]{createTernarySystem}}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@param value
#'  A \code{ternaryVariables} object, as created with 
#'  \code{\link[ternaryplot]{createTernaryVariables}}.
#'
#'
#'@return
#'  \code{ternaryVariables(s)} returns a 
#'  \code{\link[ternaryplot]{ternaryVariables-class}} object.
#'
#' 
#'@rdname ternaryVariables-methods
#'
#'@export 
#'
ternaryVariables <- function( 
 s, 
 ... 
){  
    UseMethod( "ternaryVariables" ) 
}   



#'@rdname ternaryVariables-methods
#'
#'@method ternaryVariables ternarySystem
#'
#'@export
#'
ternaryVariables.ternarySystem <- function( 
 s, 
 ... 
){  
    return( s[[ 'ternaryVariables' ]] ) 
}   



# ternaryVariables()<- ========================================== 

#'@rdname ternaryVariables-methods
#'
#'@usage ternaryVariables( s, ... ) <- value 
#'
#'@export
#'
`ternaryVariables<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "ternaryVariables<-" ) 
}   
 


#'@rdname ternaryVariables-methods
#'
#'@method ternaryVariables<- ternarySystem
#'
#'@export
#'
#'
#'@usage \method{ternaryVariables}{ternarySystem}( s, ... ) <- value
#'
`ternaryVariables<-.ternarySystem` <- function( 
 s, 
 ..., 
 value 
){  
    s[[ 'ternaryVariables' ]] <- value 
    
    ternaryCheck.ternaryVariables( s = s[[ 'ternaryVariables' ]], ... )     
    
    return( s ) 
}   



# print.ternaryVariables ========================================= 

#'Print the content of a ternaryVariables object in a human readable format.
#'
#'Print the content of a \code{ternaryVariables} object 
#'  (S3-class) in a human readable format.
#'
#'
#'@param x 
#'  A \code{ternaryVariables} object, as created with 
#'  \code{\link[ternaryplot]{createTernaryVariables}}.
#'
#'@param prefix 
#'  Single character string. Prefix used before the different 
#'  items in \code{x} (intended for internal use, for example 
#'  \code{prefix = "$ternaryGeometry"}).
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
#'@method print ternaryVariables
#' 
print.ternaryVariables <- function( 
 x, 
 prefix = "", 
 collapse = "; ", 
 ... 
){  
    cat( "A ternaryVariables (S3-class) object:\n\n" )
    
    cat( sprintf( 
        "%s$blrNames: %s\n", 
        prefix, 
        paste( x[[ "blrNames" ]], collapse = collapse ) 
    ) ) 
    
    cat( "  Names of the bottom, left and right variables\n" ) 
    cat( "  Get or set with blrNames() and blrNames() <- value\n\n" ) 
    
    cat( sprintf( 
        "%s$blrLabels: %s\n", 
        prefix, 
        paste( x[[ "blrLabels" ]], collapse = collapse ) 
    ) ) 
    
    cat( "  Labels of the bottom, left and right axis\n" ) 
    cat( "  Get or set with blrLabels() and blrLabels() <- value\n\n" ) 
    
    return( invisible( x ) ) 
}   



# print.ternarySystem ====================================== 

#'Print the content of a ternarySystem object in a human readable format.
#'
#'Print the content of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object 
#'  (S3-class) in a human readable format.
#'
#'
#'@param x 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, 
#'  as created with \code{\link[ternaryplot]{createTernarySystem}}.
#'
#'@param prefix 
#'  Single character string. Prefix used before the different 
#'  items in \code{x} (intended for internal use, for example 
#'  \code{prefix = "$ternaryGeometry"}).
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
#'@method print ternarySystem
#' 
print.ternarySystem <- function( 
 x, 
 prefix = "", 
 collapse = "; ", 
 ... 
){  
    cat( "A ternarySystem (S3-class) object:\n\n" )
    
    cat( "$ternaryVariables\n" ) 
    
    print( 
        x        = x[[ "ternaryVariables" ]], 
        prefix   = paste0( prefix, "$ternaryVariables" ), 
        collapse = collapse, 
        ... 
    )   
    
    cat( "$ternaryGeometry\n" ) 
    
    print( 
        x        = x[[ "ternaryGeometry" ]], 
        prefix   = paste0( prefix, "$ternaryGeometry" ), 
        collapse = collapse, 
        ... 
    )   
    
    cat( sprintf( "%s$main: %s\n", prefix, x[[ "main" ]] ) ) 
    cat( "  Default title of the ternary diagram\n\n" ) 
    
    cat( sprintf( "%s$vertices:\n", prefix ) ) 
    print( x[[ "vertices" ]] ) 
    cat( "  A data.frame containing the vertices identifiers and positions\n" ) 
    cat( "  Empty for a ternary-system with no classes\n" ) 
    cat( "  Can not be altered independently from $classes\n" ) 
    cat( "  Use createTernarySystem() to modify them\n\n" ) 
    
    cat( sprintf( "%s$classes:\n", prefix ) ) 
    print( x[[ "classes" ]] ) 
    cat( "  A data.frame containing the classes' abbreviations,\n" ) 
    cat( "  names and lists of vertices\n" )
    cat( "  Empty for a ternary-system with no classes\n" ) 
    cat( "  Can not be altered independently from $vertices\n" ) 
    cat( "  Use createTernarySystem() to modify them\n\n" ) 
    
    cat( sprintf( "%s$scale:\n", prefix ) ) 
    print( x[[ "scale" ]] ) 
    cat( "  A data.frame containing the min and max limits (rows)\n" ) 
    cat( "  of each axis (columns)\n" ) 
    cat( "  Currently not used (zoom feature not implemented)\n\n" ) 
    
    return( invisible( x ) ) 
}   




# ternarySystem ============================================

#'Fetch or set the ternary system definition from another object
#'
#'Fetch or set the ternary system definition from another object
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternaryPolygons-class}} object.
#'
#'@param value 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object. 
#'  The \code{ternarySystem-class} in \code{s} will be 
#'  replaced by the one in \code{value}.
#'
#'@param \dots
#'  Not used
#'
#'
#'@return 
#'  The \code{\link[ternaryplot]{ternarySystem-class}} 
#'  extracted from \code{s} or, 
#'  when using the \code{`<-`} method, a 
#'  \code{\link[ternaryplot]{ternaryPolygons-class}} object 
#'  with an updated \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'
#'@return
#'  \code{ternarySystem(s)} returns a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#' 
#'@rdname ternarySystem-methods
#'
#'@export 
#'
ternarySystem <- function( 
    s, 
    ... 
){  
    UseMethod( "ternarySystem" ) 
}   


#'@rdname ternarySystem-methods
#'
#'@method ternarySystem ternaryPolygons
#'
#'@export
#'
ternarySystem.ternaryPolygons <- function( 
    s, 
    ... 
){  
    return( attr( x = s, which = 'ternarySystem' ) ) 
}   




# ternarySystem<- ==========================================

#'@rdname ternarySystem-methods
#'
#'@export
#'
`ternarySystem<-` <- function( 
 s, 
 ..., 
 value 
){  
    UseMethod( "ternarySystem<-" ) 
}   


#'@rdname ternarySystem-methods
#'
#'@method ternarySystem<- ternaryPolygons
#'
#'@export
#'
`ternarySystem<-.ternaryPolygons` <- function( 
 s, 
 ..., 
 value 
){  
    if( !("ternarySystem" %in% class( value )) ){
        stop( "'value' is not a 'ternarySystem'-object" )
    }   
    
    #   Fetch the old ternarySystem
    oldS <- ternarySystem.ternaryPolygons( s = s, ... )
    
    #   Fetch the old and the new variable names
    oldNames <- blrNames.ternarySystem( s = oldS )
    newNames <- blrNames.ternarySystem( s = value ) 
    
    #   Rename the vertices column names in s
    for( o in 1:length( oldNames ) ){
        colnames( s )[ colnames( s ) == oldNames[ o ] ] <- 
            newNames[ o ]
    }   
    
    attr( x = s, which = "ternarySystem" ) <- value
    
    return( s )  
}   


