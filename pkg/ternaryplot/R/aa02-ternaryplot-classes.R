
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# ternaryCheck =========================================================

#'Check the validity of ternary*-class objects
#'
#'Check the validity of ternary*-class objects
#'
#'
#'@seealso Arguments \code{onFailure} and \code{okClock} in 
#'  \code{\link[ternaryplot]{getTpPar}} (package options).
#'
#'
#'@param x 
#'  A ternary*-class object, or a \code{\link{data.frame}} 
#'  containing ternary data.
#'
#'@param s 
#'  If \code{x} is a \code{\link{data.frame}}, a 
#'  \code{ternarySystem}-object, such as obtained with 
#'  \code{\link[ternaryplot]{getTernarySystem}} or output 
#'  by \code{\link[ternaryplot]{ternaryPlot}}, that defines 
#'  how the ternary data in \code{x} should be (variable 
#'  names and sum of 3 ternary fractions). Not needed if 
#'  \code{x} is not a \code{\link{data.frame}}.
#'
#'@param testRange 
#'  Single logical. Test if the range of fraction is between 0 and 
#'  the expected sum of fractions (1 or 100). 
#'
#'@param testSum 
#'  Single logical. Test if the sum of the 3 fractions is equal to 
#'  the expected sum of fractions (1 or 100).
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@rdname ternaryCheck-methods
#'
#'@export 
#'
ternaryCheck <- function(
 x, 
 ... 
){  
    UseMethod( "ternaryCheck" ) 
}   



#'@rdname ternaryCheck-methods
#'
#'@method ternaryCheck ternaryGeometry
#'
#'@export
#'
ternaryCheck.ternaryGeometry <- function(
 x, 
 # onFailure=stop, 
 ... 
){  
    valid <- TRUE 
    
    onFailure <- getTpPar( "onFailure" ) 
    
    #   Check names:
    nm <- c( "tlrAngles", "blrClock", "fracSum" )
    testNames <- nm %in% names( x ) 
    
    if( any( !testNames ) ){ 
        onFailure( sprintf( 
            "Some items (or item-labels) are missing: %s", 
            paste( nm, collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testNames ) 
    
    
    #   Check tlrAngles
    testTlrAngles <- 
        ( length( x[[ "tlrAngles" ]] ) == 3 )   & 
        is.numeric( x[[ "tlrAngles" ]] )        & 
        ( sum( x[[ "tlrAngles" ]] ) == 180 ) 
    
    
    if( !testTlrAngles ){ 
        onFailure( sprintf( 
            "'tlrAngles' must be 3 numerical values summing to 180 degrees (now %s)", 
            paste( x[[ "tlrAngles" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testTlrAngles ) 
    
    
    #   Check blrClock:
    testBlrClock <- 
        ( length( x[[ "blrClock" ]] ) == 3 )    & 
        is.logical( x[[ "blrClock" ]] ) 
    
    if( !testBlrClock ){ 
        onFailure( sprintf( 
            "'blrClock' must be 3 logical values (now %s)", 
            paste( x[[ "blrClock" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testBlrClock ) 
    
    
    okClock <- getTpPar( "okClock" ) 
    
    okClock <- unlist( lapply( 
        X        = okClock, 
        FUN      = function( X ){ 
            identical( x[[ "blrClock" ]], X ) 
        }   
    ) )  
    
    if( !any(okClock) ){   
        onFailure( "Invalid 'blrClock'. See getTpPar( 'okClock' ) for accepted values." ) 
        
        valid <- FALSE 
    };  rm( okClock )

    
    
    #   Check fracSum
    testFracSum <- 
        ( length( x[[ "fracSum" ]] ) == 1 )     & 
        is.numeric( x[[ "fracSum" ]] )          & 
        all( x[[ "fracSum" ]] %in% c( 1, 100 ) )
    
    if( !testFracSum ){ 
        onFailure( sprintf( 
            "'fracSum' must be 1 numerical values, either 1 or 100 (now %s)", 
            paste( x[[ "fracSum" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testFracSum ) 
    
    
    return( valid ) 
}   



#'@rdname ternaryCheck-methods
#'
#'@method ternaryCheck ternaryVariables
#'
#'@export
#'
ternaryCheck.ternaryVariables <- function(
 x, 
 # onFailure=stop, 
 ... 
){  
    valid <- TRUE 
    
    onFailure <- getTpPar( "onFailure" ) 
    
    #   Check names:
    nm <- c( "blrNames", "blrLabels" )
    testNames <- nm %in% names( x ) 
    
    if( any( !testNames ) ){ 
        onFailure( sprintf( 
            "Some items (or item-labels) are missing: %s", 
            paste( nm, collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testNames ) 
    
    
    #   Check blrNames
    testBlrNames <- 
        ( length( x[[ "blrNames" ]] ) == 3 )   & 
        is.character( x[[ "blrNames" ]] ) 
    
    
    if( !testBlrNames ){ 
        onFailure( sprintf( 
            "'blrNames' must be 3 character strings (now %s)", 
            paste( x[[ "blrNames" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testBlrNames ) 
    
    
    #   Check blrLabels
    testBlrLabels <- 
        ( length( x[[ "blrLabels" ]] ) == 3 )   & 
        ( class( x[[ "blrLabels" ]] ) %in% c( "character", "expression", "name", "call" ) ) 
    
    
    if( !testBlrLabels ){ 
        onFailure( sprintf( 
            "'blrLabels' must be an object of class character, expression, name or call and length 3 (now %s)", 
            paste( x[[ "blrLabels" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testBlrLabels ) 
    
    
    return( valid ) 
}   



#'@rdname ternaryCheck-methods
#'
#'@method ternaryCheck ternarySystem
#'
#'@export
#'
ternaryCheck.ternarySystem <- function(
 x, 
 # onFailure=stop, 
 ... 
){  
    valid <- TRUE 
    
    onFailure <- getTpPar( "onFailure" ) 
    
    #   Check names:
    nm <- c( "ternaryGeometry", "ternaryVariables", "main", 
        "vertices", "classes", "scale", "over" )
    testNames <- nm %in% names( x ) 
    
    if( any( !testNames ) ){ 
        onFailure( sprintf( 
            "Some items (or item-labels) are missing: %s", 
            paste( nm[ !testNames ], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testNames ) 
    
    
    valid <- ternaryCheck( x[[ "ternaryGeometry" ]], onFailure = onFailure, ... )
    valid <- ternaryCheck( x[[ "ternaryVariables" ]], onFailure = onFailure, ... )
    
    
    #   Check main
    testMain <- 
        ( length( x[[ "main" ]] ) %in% c(1,0) )   & 
        ( class( x[[ "main" ]] ) %in% c( "character", "expression", "name", "call" ) ) 
    
    if( !testMain ){ 
        onFailure( sprintf( 
            "'main' must be an object of class character, expression, name or call and length 1 (now %s)", 
            paste( x[[ "main" ]], collapse = "; " ) 
        ) ) 
        
        valid <- FALSE 
    };  rm( testMain ) 
    
    
    #   Check vertices
    verticesDefault <- getTpPar( "vertices" ) 
    
    cn <- c( colnames(verticesDefault)[1], 
        x[[ "ternaryVariables" ]][[ "blrNames" ]] )
    
    testCol <- cn %in% colnames( x[[ "vertices" ]] ) 
    
    if( !all( testCol ) ){ 
        onFailure( sprintf( 
            "Some columns are missing in 'vertices': %s", 
            paste( cn[ !testCol ], collapse = "; " )
        ) ) 
        
        valid <- FALSE 
    };  rm( testCol ) 
    
    
    #   Test classes:
    cn2 <- colnames( getTpPar( "classes" ) ) 
    
    testCol <- cn2 %in% colnames( x[[ "classes" ]] ) 
    
    if( !all( testCol ) ){ 
        onFailure( sprintf( 
            "Some columns are missing in 'classes': %s", 
            paste( cn2[ !testCol ], collapse = "; " )
        ) ) 
        
        valid <- FALSE 
    };  rm( testCol ) 
    
    
    #   Test class x vertices
    verticesId <- unlist( x[[ "classes" ]][, "verticesId" ] ) 
    id         <- x[[ "vertices" ]][, "id" ] 
    testClaVer <- verticesId %in% id 
    
    if( any( !testClaVer ) ){ 
        onFailure( sprintf( 
            "Some classes[, 'verticesId' ] are missing in vertices[, 'id']: %s", 
            paste( verticesId[ !testClaVer ], collapse = "; " )
        ) ) 
        
        valid <- FALSE 
    };  rm( testClaVer )
    
    
    testClaVer2 <- id %in% verticesId  
         
    
    if( any( !testClaVer2 ) ){ 
        onFailure( sprintf( 
            "Some vertices[, 'id'] are missing in classes[, 'verticesId' ]: %s", 
            paste( id[ !testClaVer2 ], collapse = "; " )
        ) ) 
        
        valid <- FALSE 
    };  rm( testClaVer2 )
    
    
    #   Test scale:
    testCol <- cn[ -1 ] %in% colnames( x[[ "scale" ]] ) 
    
    if( !all( testCol ) ){ 
        onFailure( sprintf( 
            "Some columns are missing in 'scale': %s", 
            paste( cn[ -1 ][ !testCol ], collapse = "; " )
        ) ) 
        
        valid <- FALSE 
    };  rm( testCol ) 
    
    testRow <- 
        ( nrow( x[[ "scale" ]] ) == 2L ) & 
        all( c( "min", "max" ) %in% rownames( x[[ "scale" ]] ) ) 
    
    if( !testRow ){ 
        onFailure( "scale must have two rows, labelled 'min' and 'max'" ) 
        
        valid <- FALSE 
    };  rm( testRow ) 
    
    
    testDiff <- as.numeric( x[[ "scale" ]][ 2, ] - x[[ "scale" ]][ 1, ] ) 
    testDiff <- all( testDiff == testDiff[1] ) 
    
    if( any( !testDiff ) ){  
        onFailure( "In 'scale', the difference between min and max must be identical" ) 
        
        valid <- FALSE 
    }   
    
    
    #   Test the overlay function
    if( !is.null( x[[ "over" ]] ) ){
        if( !("function" %in% class(x[[ "over" ]])) ){
            onFailure( sprintf( 
                "If not 'NULL', item 'over' must be a function (now: %s)", 
                paste( class(x[[ "over" ]]), collapse = "; " )
            ) ) 
            
            valid <- FALSE 
        }else{
            expectArgs <- c( "s", "x", "scale" )
            
            testExpectArgs <- expectArgs %in% names( formals( x[[ "over" ]] ) ) 
            
            if( any( !testExpectArgs ) ){
                onFailure( sprintf( 
                    "Some arguments expected for function defined by item 'over' are missing (%s)", 
                    paste( expectArgs[ !testExpectArgs ], collapse = "; " )
                ) ) 
                
                valid <- FALSE 
            }   
            
            rm( expectArgs, testExpectArgs )
        }   
        
    }   
    
    return( valid ) 
}   




# ternaryGeometry-class ====================================

#'Class to hold geometrical settings of a ternary plot (graphics)
#'
#'Class (\code{S3}) to hold geometrical settings of a ternary 
#'  plot (graphics). A \code{ternaryGeometry}-class object is 
#'  a tagged \code{\link[base]{list}} of several 
#'  objects/items (see below) that describes the geometry 
#'  of a ternary plot (ternary diagram, ternary triangle). 
#'  It describes the triangle's angles and the direction of 
#'  its axis (see below). A 'tagged' list means a list of 
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
#'\itemize{
#'  \item \bold{\code{tlrAngles}}: Vector of 3 numerical 
#'    values. The angles of the \bold{t}op, \bold{l}eft, and 
#'    \bold{r}ight vertices of the triangle. In degrees. 
#'    The sum of the 3 angles should thus always be 180.
#'  \item \bold{\code{blrClock}}: Vector of 3 logical/boolean 
#'    values (one \code{\link[base]{NA}}-value is allowed). 
#'    The direction of the \bold{b}ottom, \bold{l}eft, and 
#'    \bold{r}ight axis. \code{TRUE} when an axis is clockwise 
#'    (i.e. the values on the axis increase clockwise), 
#'    \code{FALSE} when it is counter-clockwise (i.e. the 
#'    values on the axis increase counter-clockwise) and 
#'    \code{\link[base]{NA}} when the values increase when 
#'    moving away from the axis (inside the triangle; in with 
#'    lines parallel to the axis being isolines, for that 
#'    axis). Possible combinations are \code{c(TRUE,TRUE,TRUE)}, 
#'    \code{c(FALSE,FALSE,FALSE)}, \code{c(FALSE,TRUE,NA)} 
#'    and \code{c(TRUE,NA,FALSE)}.
#'  \item \bold{\code{fracSum}}: Single numerical value. 
#'    Sum of the 3 fractions. Possible values are \code{1} 
#'    (when values/units are fractions) or \code{100} (when 
#'    values/units are percentages).
#'} 
#'
#'
#'@seealso
#'\itemize{
#'  \item \code{\link[ternaryplot]{ternarySystem-class}} 
#'   (full ternary data and plot definition), that builds 
#'    on \code{ternaryGeometry-class}, 
#'    \code{\link[ternaryplot]{ternaryVariables-class}} and 
#'    other settings.
#'  \item \code{\link[ternaryplot]{createTernaryGeometry}} 
#'    for setting up a \code{ternaryGeometry-class} object.
#'  \item \code{\link[ternaryplot]{tlrAngles}} for fetching 
#'    or setting the geometry's angle, 
#'    \code{\link[ternaryplot]{blrClock}} for fetching 
#'    or setting the geometry axis' directions and 
#'    \code{\link[ternaryplot]{fracSum}} for fetching 
#'    or setting the geometry's sum of fractions.
#'  \item \code{\link[ternaryplot]{ternaryGeometry}} 
#'    for fetching or changing the geometry of a 
#'    \code{ternarySystem-class} object.
#'  \item \code{\link[ternaryplot]{ternaryCheck}}  
#'    for checking the validity of a \code{ternaryGeometry-class} 
#'    object.
#'} 
#'
#'
#'@name ternaryGeometry-class
#'
#'@rdname ternaryGeometry-class
#'
NULL 




# createTernaryGeometry ============================================

## # Function that generates the 2ndary class of ternaryGeometry 
## # object after the blrClock-argument.
.generateTernaryGeometry2ndClass <- function( 
    s, 
    class1 = "ternaryGeometry"
){  
    .blrClock <- blrClock( s ) 
    
    # if( !("logical" %in% class( blrClock )) ){
        # sprintf(
            # "'blrClock' is not a logical (but: %s).", 
            # paste( class( blrClock ), collapse = "; " )
        # )   
    # }   
    
    if( !any( is.na( .blrClock ) ) ){
        if( all( .blrClock ) ){
            class2 <- "geo_TTT"
            
        }else if( all( !.blrClock ) ){
            class2 <- "geo_FFF"
            
        }else{
            class2 <- character(0)
            
        }   
    }else{
        if( identical( .blrClock, c( FALSE, TRUE, NA ) ) ){ 
            class2 <- "geo_FTX"
            
        }else if( identical( .blrClock, c( TRUE, NA, FALSE ) ) ){
            class2 <- "geo_TXF"
            
        }else{
            class2 <- character(0)
            
        }   
    }   
    
    class( s ) <- c( class1, class2 )
    
    return( s )
}   


#'Creates a ternaryGeometry-class object (settings of a ternary plot's geometry).
#'
#'Creates a \code{\link[ternaryplot]{ternaryGeometry-class}} 
#'  object (settings of a ternary plot's geometry).
#'
#'
#'@param tlrAngles
#'  See \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'
#'@param blrClock
#'  See \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'
#'@param fracSum
#'  See \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}
#'
#'
#'@return
#'  A a \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'
#'
#'@seealso \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'
#'
#'@example inst/examples/createTernaryGeometry-example.R
#'
#'
#'@rdname createTernaryGeometry
#'
#'@export
#'
createTernaryGeometry <- function(
 tlrAngles  = c( 60, 60, 60 ), 
 blrClock   = rep( TRUE, 3 ), 
 fracSum    = 100,  
 ...
){  
    #   Create a ternary geometry object:
    tg <- list( 
        "tlrAngles" = tlrAngles, 
        "blrClock"  = blrClock, 
        "fracSum"   = fracSum 
    )   
    
    
    #   Set the class
    # class( tg ) <- .generateTernaryGeometry2ndClass( 
        # blrClock = blrClock ) 
    
    class( tg ) <- "ternaryGeometry"
    
    
    #   Check:
    ternaryCheck( tg, ... )

    
    
    return( tg ) 
}   




# ternaryVariables-class ===================================

#'Class to hold the variables' names and labels of ternary plots and ternary data.
#'
#'Class (\code{S3}) to hold the variables' names and labels 
#'  of ternary plots (graphics) and ternary data. A 
#'  \code{ternaryVariables}-class object is 
#'  a tagged \code{\link[base]{list}} of several 
#'  objects/items (see below). By variables' names is meant 
#'  the name of the variables in a ternary data (column names), 
#'  and by variables' labels is meant the axis labels on a 
#'  ternary plot. A 'tagged' list means a list of 
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
#'\itemize{
#'  \item \bold{\code{blrNames}}: Vector of 3 character strings. 
#'    column names of the variables to be displayed on the 
#'    \bold{b}ottom, \bold{l}eft, and \bold{r}ight axis of a 
#'    ternary plot. If \code{blrNames} is set to 
#'    \code{c( "_bottom_","_left_","_right_" )}, 
#'    \code{ternarySystem}s based on the set of 
#'    \code{ternaryVariables} will be \emph{undefined}, meaning 
#'    that any variable name goes. In the case of ternary 
#'    data points, it means that the first three columns 
#'    will be used independently of how they are called.
#'  \item \bold{\code{blrLabels}}: Vector of 3 character 
#'    strings or vector of 3 expressions.\bold{b}ottom, 
#'    \bold{l}eft, and \bold{r}ight axis' labels of a ternary 
#'    plots. If \code{blrNames} is set 
#'    to \code{c( "_bottom_","_left_","_right_" )}, and 
#'    \code{blrLabels} to \code{rep( NA_character_, 3 )}, 
#'    \code{\link[ternaryplot]{ternaryPlot}} and 
#'    \code{\link[ternaryplot]{ternaryPoints}} use the column 
#'    names of the dataset that is provided (if provided).
#'} 
#'
#'
#'@seealso
#'\itemize{
#'  \item \code{\link[ternaryplot]{ternarySystem-class}} 
#'   (full ternary system definition), that builds on 
#'    \code{\link[ternaryplot]{ternaryGeometry-class}}, 
#'    \code{ternaryVariables-class} and other settings.
#'  \item \code{\link[ternaryplot]{createTernaryVariables}} 
#'    for setting up a \code{ternaryVariables-class} object.
#'  \item \code{\link[ternaryplot]{blrNames}} for fetching 
#'    or setting the variables names and 
#'    \code{\link[ternaryplot]{blrLabels}} for fetching 
#'    or setting the axis labels.
#'  \item \code{\link[ternaryplot]{ternaryCheck}}  
#'    for checking the validity of a 
#'    \code{ternaryVariables-class} object.
#'} 
#'
#'
#'@name ternaryVariables-class
#'
#'@rdname ternaryVariables-class
#'
NULL 




# createTernaryVariables ===========================================

#'Creates a ternaryVariables-class object (names and labels of ternary data and ternary plot)
#'
#'Creates a ternaryVariables-class object (names and labels 
#'  of ternary data and ternary plot)
#'
#'
#'@param blrNames
#'  See \code{\link[ternaryplot]{ternaryVariables-class}}.
#'
#'@param blrLabels
#'  See \code{\link[ternaryplot]{ternaryVariables-class}}.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}
#'
#'
#'@return
#'  A \code{\link[ternaryplot]{ternaryVariables-class}}.
#'
#'
#'@seealso \code{\link[ternaryplot]{ternaryVariables-class}}.
#'
#'
#'@example inst/examples/createTernaryVariables-example.R
#'
#'@rdname createTernaryVariables
#'
#'@export 
#'
createTernaryVariables <- function(
    blrNames   = c( "_bottom_", "_left_", "_right_" ), 
    blrLabels  = rep( NA_character_, 3 ), 
    ...
){  
    tv <- list( 
        "blrNames"  = blrNames, 
        "blrLabels" = blrLabels 
    )   
    
    #   Set the class
    class( tv ) <- "ternaryVariables"
    
    
    #   Check:
    ternaryCheck( tv, ... )
    
    
    return( tv ) 
}   




# ternarySystem-class ======================================

#'Class to hold the definition of a ternarySystem (the full definition of ternary plot and optionally classification).
#'
#'Class (\code{S3}) to hold the definition of a ternarySystem 
#'  (the full definition of ternary plot and optionally 
#'  classification). A \code{ternarySystem-class} object is 
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
#'\itemize{
#'  \item \bold{\code{ternaryGeometry}}: See 
#'    \code{\link[ternaryplot]{ternaryGeometry-class}}.
#'  \item \bold{\code{ternaryVariables}}: See 
#'    \code{\link[ternaryplot]{ternaryVariables-class}}.
#'  \item \bold{\code{main}}: Single character string. 
#'    Title of the triangle plot.
#'  \item \bold{\code{vertices}}: See 
#'    \code{\link[ternaryplot]{tpPar}}. If non-null, 
#'    \code{\link[base]{data.frame}} with 4 columns: \code{id}, 
#'    and 3 other columns corresponding to \code{blrNames} 
#'    in \code{ternaryVariables}. If \code{NULL}, default 
#'    values will be used \code{getTpPar("vertices")}, and 
#'    the columns names changed to those of 
#'    \code{ternaryVariables}.
#'  \item \bold{\code{classes}}: See 
#'    \code{\link[ternaryplot]{tpPar}}. Please keep in mind 
#'    that the order of the class matters regarding the 
#'    classification of point ternary data, in the case of 
#'    "tights" (points that are in between two or more 
#'    classes): the function \code{ternaryClassify} uses 
#'    internally the function \code{\link[sp]{over}}, for 
#'    which the last polygon in which a point falls is 
#'    returned.
#'  \item \bold{\code{scale}}: NOT USED (YET). See 
#'    \code{\link[ternaryplot]{tpPar}}. If non-null, 
#'    \code{\link[base]{data.frame}} with 3 columns, 
#'    corresponding to \code{blrNames} in 
#'    \code{ternaryVariables}. If \code{NULL}, default values 
#'    will be used \code{getTpPar("scale")}, and the 
#'    columns names changed to those of \code{ternaryVariables}.
#'  \item \bold{\code{over}}: Either \code{NULL} or a 
#'    \code{\link{function}} with 3 arguments, \code{s}, 
#'    code{x} and \code{scale}. If a \code{\link{function}}, 
#'    should be used to add arbitrary graphical overlay on 
#'    top of ternary plots. Experimental.
#'} 
#'
#'
#'@seealso
#'\itemize{
#'  \item \code{\link[ternaryplot]{getTernarySystem}} to fetch 
#'    an existing (pre defined) ternary system, or 
#'    \code{\link[ternaryplot]{createTernarySystem}} to create 
#'    one. Notice that \code{\link[ternaryplot]{ternaryPlot}} 
#'    also output a \code{ternarySystem-class} object.
#'  \item \code{\link[ternaryplot]{ternaryPlot}} for plotting 
#'    a ternary system (with its ternary classification, when 
#'    relevant, and optionally with ternary data points), 
#'    \code{\link[ternaryplot]{ternaryClasses}} for extracting 
#'    the classes of a \code{ternarySystem-class} (as 
#'    \code{ternaryPolygons}) and \code{ternaryClassify} 
#'    for classifying ternary data.
#'  \item \code{\link[ternaryplot]{ternaryGeometry-class}} and 
#'    \code{\link[ternaryplot]{ternaryVariables-class}}, 
#'    two essentials building blocks of ternary systems.
#'  \item \code{\link[ternaryplot]{blrNames}} for fetching 
#'    or setting the variables names and 
#'    \code{\link[ternaryplot]{blrLabels}} for fetching 
#'    or setting the axis labels.
#'  \item \code{\link[ternaryplot]{tlrAngles}} for fetching 
#'    or setting the geometry's angle, 
#'    \code{\link[ternaryplot]{blrClock}} for fetching 
#'    or setting the geometry axis' directions and 
#'    \code{\link[ternaryplot]{fracSum}} for fetching 
#'    or setting the geometry's sum of fractions.
#'  \item \code{\link[ternaryplot]{ternaryCheck}}  
#'    for checking the validity of a 
#'    \code{ternarySystem-class} object.
#'} 
#'
#'
#'@name ternarySystem-class
#'
#'@rdname ternarySystem-class
#'
NULL 




# createTernarySystem ===========================================

#'Creates a ternarySystem-class object (the full definition of ternary plot and optionally classification).
#'
#'Creates a ternarySystem-class object (the full definition 
#'  of ternary plot and optionally classification).
#'
#'
#'@param ternaryGeometry
#'  See \code{\link[ternaryplot]{ternarySystem-class}}, 
#'  \code{\link[ternaryplot]{ternaryGeometry-class}}, and 
#'  \code{\link[ternaryplot]{createTernaryGeometry}}.
#'
#'@param ternaryVariables
#'  See \code{\link[ternaryplot]{ternarySystem-class}}, 
#'  \code{\link[ternaryplot]{ternaryVariables-class}}, and 
#'  \code{\link[ternaryplot]{createTernaryVariables}}.
#'
#'@param main 
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param vertices
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param classes
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param scale
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param over
#'  See \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[ternaryplot]{ternaryCheck}}
#'
#'
#'@return 
#'  A \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'
#'@seealso \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'
#'@example inst/examples/createTernarySystem-example.R
#'
#'@rdname createTernarySystem
#'
#'@export
#'
createTernarySystem <- function(
    ternaryGeometry = NULL, 
    ternaryVariables = NULL, 
    main = character(0), 
    vertices = NULL, 
    classes = NULL, 
    # labels = NULL, 
    scale = NULL, 
    over = NULL, 
    ...
){  
    tsy <- list() 
    
    if( is.null( ternaryGeometry ) ){ 
        tsy[[ "ternaryGeometry" ]] <- createTernaryGeometry( ... )
    }else{ 
        tsy[[ "ternaryGeometry" ]] <- ternaryGeometry 
    }   
    
    if( is.null( ternaryVariables ) ){ 
        tsy[[ "ternaryVariables" ]] <- createTernaryVariables( ... )
    }else{ 
        tsy[[ "ternaryVariables" ]] <- ternaryVariables
    }   
    
    tsy[[ "main" ]] <- main
    
    
    #   Expected columns
    verticesDefault <- getTpPar( "vertices" ) 
    
    cn <- c( colnames(verticesDefault)[1], 
        tsy[[ "ternaryVariables" ]][[ "blrNames" ]] )
    
    if( is.null( vertices ) ){ 
        tsy[[ "vertices" ]] <- verticesDefault 
        
        colnames( tsy[[ "vertices" ]] ) <- cn
    }else{ 
        tsy[[ "vertices" ]] <- vertices 
    }   
    
    if( is.null( classes ) ){ 
        tsy[[ "classes" ]] <- getTpPar( "classes" ) 
    }else{ 
        tsy[[ "classes" ]] <- classes 
        
        # cn2 <- colnames( getTpPar( "classes" ) ) 
    }   
    
    # if( is.null( labels ) ){ 
        # if( !is.null( classes ) ){
            # if( "abbrev" %in% colnames( classes ) ){
                # labels <- 
            # }   
        # }   
    # }else{ 
    # }   
    
    if( is.null( scale ) ){ 
        tsy[[ "scale" ]] <- getTpPar( "scale" ) 
        
        colnames( tsy[[ "scale" ]] ) <- cn[ -1 ]
    }else{ 
        tsy[[ "scale" ]] <- scale 
    }   
    
    
    #   Overlay function
    tsy[ "over" ] <- list( over )
    
    
    #   Set the class
    class( tsy ) <- "ternarySystem"
    
    # class( tsy ) <- .generateTernaryGeometry2ndClass( 
        # blrClock = blrClock( tsy ), class1 = "ternarySystem" ) 
    
    
    #   Check:
    ternaryCheck( tsy, ... ) 
    
    
    return( tsy ) 
}   



# .fixTernarySystem ========================================

## # Adjust the ternaryVariables to the case where variables are undefined
## # 
## # Adjust the ternaryVariables to the case where variables 
## # are undefined. If the triangle is undetermined, attribute 
## # to the bottom-left-right variables the names of the 
## # first 3 variables in x.
## #
## #
## #@param s
## #  See ternary2xy
## #
## #@param x
## #  See ternary2xy
## #
## #
## #@return 
## # An updated version of 's', adapted to 'x'.
## #
.fixTernarySystem <- function(
 s, 
 x  
){  
    blrNames0 <- blrNames( s = s )
    
    #   Check if the triangle is undefined
    if( all( blrNames0 == c( "_bottom_", "_left_", "_right_" ) ) ){
        #   Check if x has at least 3 columns:
        if( ncol( x ) < 3L ){
            stop( sprintf(
                "'s' is an undefined triangle (blrNames(s) is '_bottom_', '_left_', '_right_'), but 'x' has less than 3 columns (%s).", 
                ncol( x ) 
            ) ) 
        }else{
            blrNames( s = s )  <- colnames( x )[ 1L:3L ] 
            
            #   Set the labels if they are NA
            if( all( is.na( blrLabels( s = s ) ) ) ){
                blrLabels( s = s ) <- colnames( x )[ 1L:3L ] 
            }   
        }   
    }   
    
    return( s ) 
}   

