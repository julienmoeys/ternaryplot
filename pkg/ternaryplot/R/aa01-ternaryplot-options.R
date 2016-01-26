
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                   |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# ===============================================================
# Create two environment that will contain the package parameters

# - Backup / reference 

#' Environment containng a backup copy of all ternaryplot package-options
#'
#' Environment containing a backup copy of all ternaryplot 
#'  package-options
#'
#'
#' @format 
#'  An environment, where each item corresponds to an 
#'  argument in \code{\link[ternaryplot]{tpPar}}.
#'
#'
#'@rdname tpParList-environment-backup
#'
#'@keywords internal
#'
.tpParList <- new.env() 

# - User visible container

#' Environment containing all ternaryplot package-options
#'
#' Environment containing all ternaryplot package-options
#'
#'
#' @format 
#'  An environment, where each item corresponds to an 
#'  argument in \code{\link[ternaryplot]{tpPar}}.
#'
#'
#'@rdname tpParList-environment
#'
#'@export
#'
#'@keywords internal
#'
tpParList  <- new.env() 



# Set some default parameters: 

# NON-GRAPHICAL PARAMETERS
# ========================

.tpParList[[ "testSum" ]]       <- TRUE 

.tpParList[[ "testRange" ]]     <- TRUE 

.tpParList[[ "fracSumTol" ]]    <- 1/1000 

.tpParList[[ "vertices" ]]      <- data.frame( 
    "id"       = integer(0), 
    "_bottom_" = numeric(0), 
    "_left_"   = numeric(0), 
    "_right_"  = numeric(0)  
)   

.tpParList[[ "classes" ]]       <- data.frame( 
    "abbrev"     = character(0), 
    "name"       = character(0),  
    "verticesId" = I( vector( length = 0, mode = "list" ) ),
    stringsAsFactors = FALSE 
)   

.tpParList[[ "scale" ]]         <- data.frame( 
    "_bottom_" = c( 000, 100 ),  
    "_left_"   = c( 000, 100 ),  
    "_right_"  = c( 000, 100 ), 
    row.names  = c( "min", "max" ) 
)   

.tpParList[[ "okClock" ]]       <- list( 
    #       #    Bottom Left    Right  
    "TTT"   = c( TRUE,  TRUE,   TRUE    ), 
    "FFF"   = c( FALSE, FALSE,  FALSE   ), 
    "TXF"   = c( TRUE,  NA,     FALSE   ), 
    "FTX"   = c( FALSE, TRUE,   NA      )  
   #"XFT"   = c( NA,    FALSE,  TRUE    )  # Un-tested
)   

.tpParList[[ "sp" ]]            <- TRUE 

.tpParList[[ "onFailure" ]]     <- stop 

# GRAPHICAL PARAMETERS
# ====================

# Plot region (frame)
# -------------------

.tpParList[[ "plot.bg" ]]       <- NA 

# Axis
# ----

.tpParList[[ "ticksAt" ]]       <- seq( from = 0, to = 1, by = .1 ) 

.tpParList[[ "ticksShift" ]]    <- NA_real_ # 0.040 

.tpParList[[ "ticksLabelsShift" ]] <- NA_real_ 

.tpParList[[ "arrowsShift" ]]   <- rep( NA_real_, 2 ) # c( 0.075, 0.125 )
.tpParList[[ "arrowsHeight" ]]  <- 0.75

.tpParList[[ "arrowsCoords" ]]  <- c( .15, .45, .45, .55 ) 

.tpParList[[ "arrowsBreak" ]]   <- TRUE 

.tpParList[[ "arrowsLength" ]]  <- NA_real_ 

.tpParList[[ "axis.line.lwd" ]] <- NULL 
.tpParList[[ "axis.line.col" ]] <- NULL 

.tpParList[[ "tick.line.lwd" ]] <- NULL 
.tpParList[[ "tick.line.col" ]] <- NULL 

# Grid
# ----

.tpParList[[ "grid.line.col" ]] <- "lightgray" 
.tpParList[[ "grid.line.lwd" ]] <- NULL 

# Classes
# -------

.tpParList[[ "class.label.col" ]]  <- "darkgray"
.tpParList[[ "class.border.col" ]] <- "darkgray"
.tpParList[[ "class.bg" ]]         <- NA 
.tpParList[[ "class.border.lwd" ]] <- NULL 



# tpPar =========================================================

#'Get or set default parameters for the package.
#'
#'Get or set default parameters for the package. Notice changes 
#'  done to the parameter values are reset everytime the R session 
#'  is closed and the package is reloaded.
#'
#'
#'@details 
#'  The function has 3 possible, non-exclusive behaviours: \itemize{ \item If
#'  \code{reset=TRUE}, resetting the parameters to their initial values, as
#'  defined in this function. \item (Silently) returning the actual value of the
#'  package parameters. If \code{par=NULL}, all the values are returned.  If
#'  \code{par} is a vector of parameter names, their value will be returned.
#'  \item Setting-up the value of some parameters, passing a list of parameter
#'  value to \code{par} OR setting some of the parameters listed above. }
#'
#'  Notice that when \code{reset=TRUE} and some new parameter values are
#'  provided, the parameters are first reset, and then the new parameter values
#'  are set. If \code{par} is a list, parameters are set first according to
#'  values in \code{par}, and then according to values in the parameters listed
#'  below. This combination is not recommended, but nonetheless possible.
#'
#'  The actual value of the parameters is stored in (and can be retrieved from)
#'  the environment \code{rspPars}. The default value of the parameters are
#'  stored in the environment \code{rspPars}. Do not use them directly.
#'
#'
#'@param par  
#'  Three possible cases: \itemize{ \item If \code{par} is \code{NULL}
#'  (default): All the actual value of the parameters will be silently returned.
#'  \item If \code{par} is a vector of character strings representing parameter
#'  names. The value of the parameters named here will be (silently) returned.
#'  \item If \code{par} is a list following the format \code{tag = value}, where
#'  \code{tag} is the name of the parameter to be changed, and \code{value} is
#'  its new value.  Such a list is returned by \code{tpPar()}. Notice that
#'  parameters can also be set indivudually, using the options listed below. }
#'
#'@param reset 
#'  Single logical. If TRUE, all the parameters will be set to their
#'  default value. Values are reset before any change to the parameter values, as
#'  listed below.
#'
#'@param testRange 
#'  Single logical. Test if the range of fraction is between 0 and 
#'  the expected sum of fractions (1 or 100). 
#'
#'@param testSum 
#'  Single logical. Test if the sum of the 3 fractions is equal to 
#'  the expected sum of fractions (1 or 100).
#'
#'@param fracSumTol 
#'  Single numeric. Tolerance on the sum of the 3 ternary fractions. Overall 
#'  tolerance is \code{fracSum * fracSumTol}, where \code{fracSum} is the 
#'  expected sum of the 3 ternary fractions. See 
#'  \code{\link[ternaryplot]{createTernaryGeometry}} or 
#'  \code{\link[ternaryplot]{createTernarySystem}}.
#'
#'@param ticksAt 
#'  Vector of numeric. Pre-defined position of the tick-marks for the 3 axis.
#'  Between 0 and 'fracSum' (the sum of the 3 fractions).
#'
#'@param ticksShift
#'  Single numeric. Tick-marks 'size', expressed so that 
#'  \code{ticksShift * fracSum} is the length of the tick-marks.
#'  If \code{NA}, it is calculated internally from 
#'  \code{par("tcl")} and the height in of a margin line 
#'  in inches, estimated using the internal function 
#'  \code{.nbMargin2diffXY()}.
#'
#'@param ticksLabelsShift
#'  Single numeric. Tick-label-marks 'size'.
#'  If \code{NA}, it is calculated internally from 
#'  \code{par("mgp")} and the height in of a margin line 
#'  in inches, estimated using the internal function 
#'  \code{.nbMargin2diffXY()}.
#'
#'@param arrowsShift
#'  Vector of two numeric values. Axis' arrows' shift from their 
#'  axis, expressed so that \code{arrowsShift * fracSum} is the 
#'  start and end point. If \code{NA}, the arrow shift from 
#'  their axis will be calculated from \code{par("mgp")[ 1L ]} 
#'  and \code{arrowsHeight} (below).
#'
#'@param arrowsHeight
#'  Single numeric values. Axis' arrows' height (distance 
#'  between the 1st part of the arrow and the 2nd part of the 
#'  arrow), expressed in fraction of margin-lines-height 
#'  (same as \code{par("mgp")}). Only used when \code{arrowsShift} 
#'  (above) is \code{NA}.
#'
#'@param vertices
#'  Vertices of a ternary classification (default): a 
#'  \code{\link[base]{data.frame}} with 4 columns \code{id}, 
#'  \code{bo}, \code{le} and \code{ri}, as the identifier and 
#'  the the 3 fractions (bottom, left, right) of the vertices. 
#'  Each row is a vertex.
#'
#'@param classes
#'  Polygons (classes outline) of a ternary classification (default): 
#'  a \code{\link[base]{data.frame}} with 3 columns \code{abbrev}, 
#'  \code{name} and \code{verticesId}, as the abbreviation, 
#'  name and identifier of the vertices of each class. Notice 
#'  that \code{verticesId} must be a \code{\link[base]{list}} of 
#'  vectors, each containing the vertices that define the polygon. 
#'  You can use \code{\link[base]{list}}\code{()} to preserve 
#'  the list format when defining the \code{\link[base]{data.frame}}.
#'  For example 
#'  \code{ data.frame( "abbrev" = "A", "name" = "Aa", "verticesId" = I( list( 1:3 ) ) ) }
#'
#'@param scale
#'  Scale-extent of a ternary classification (default): a 
#'  \code{\link[base]{data.frame}} with 3 columns \code{bo}, 
#'  \code{le} and \code{ri}, and 2 rows (\code{min} and \code{max}), 
#'  as the min and max of the 3 fractions to be displayed (bottom, 
#'  left, right).
#'
#'@param okClock
#'  A list of vectors of 3 logical values, with the valid 
#'  \code{blrClock} geometries.
#'
#'@param sp 
#'  Single logical value. If \code{TRUE}, the low-level graphic 
#'  functions output a \code{Spatial*} object of the graphical 
#'  element that can be reused in later calculations with 
#'  \code{\link[sp]{sp}}. If \code{FALSE}, simply returns a 
#'  \code{\link[base]{data.frame}} with the x-y coordinates of the 
#'  graphical element.
#'
#'@param onFailure
#'  R \code{\link{function}}. Function that should be used by 
#'  \code{\link[ternaryplot]{ternaryCheck}} (and related methods)
#   when a non-conformity is found. Default value is \code{\link{stop}}, 
#   but can be changed to \code{\link{warning}} or even 
#   \code{\link{message}} (at the user's own risk).
#'
#'@param grid.line.col
#'  Single character value representing a color. Color of the 
#'  grid-lines added to a ternary plot.
#'
#'@param grid.line.lwd
#'  Single numerical value. Thickness of the grid-lines 
#'  added to a ternary plot.
#'
#'@param arrowsCoords 
#'  Parameters used internally to define axis-arrows location
#'
#'@param arrowsBreak
#'  Single logical value. If \code{TRUE}, axis-arrows are 'broken' 
#'  (i.e. with the arrow starting parallel to the axis and finishing 
#'  toward the axis). 
#'
#'@param arrowsLength
#'  Single numerical value. Length of the arrows' head (see 
#'  \code{length}-argument in \code{\link[graphics]{arrows}}). 
#'  in inches. If \code{NA}, calculated internally so that 
#'  the length of the arrows' head is proportional to 
#'  the plot dimension (see argument \code{pin} in 
#'  \code{\link[graphics]{par}}).
#'
#'@param axis.line.lwd
#'  Single numerical value. Line thickness for the axis-lines 
#'  (including arrow lines)
#'
#'@param axis.line.col
#'  Single character string representing a colour. Colour 
#'  of the axis lines (including arrow lines)
#'
#'@param ticks.line.lwd
#'  Single numerical value. Line thickness for the axis ticks 
#'  lines.
#'
#'@param ticks.line.col
#'  Single character string representing a colour. Colour 
#'  of the axis tick lines.
#'
#'@param plot.bg
#'  Single character value representing a colour. Fill-colour of the 
#'  plot region (frame). Set to \code{NA} or \code{"transparent"} 
#'  to suppress color.
#'
#'@param class.label.col 
#'  Vector of character strings representing colours (see for 
#'  example \code{\link[grDevices]{colours}}). Colours of the 
#'  labels of ternary class polygons (when displayed on 
#'  a ternary plot)
#'
#'@param class.border.col 
#'  Vector of character strings representing colours (see for 
#'  example \code{\link[grDevices]{colours}}). Colours of the 
#'  lines of ternary class polygons (when displayed on 
#'  a ternary plot)
#'
#'@param class.bg
#'  Vector of character strings representing colours (see for 
#'  example \code{\link[grDevices]{colours}}). Colours of the 
#'  background (filling) of ternary class polygons (when 
#'  displayed on a ternary plot)
#'
#'@param class.border.lwd
#'  Vector of numerical values. Line width of ternary class 
#'  polygons (when displayed on a ternary plot)
#'
#'
#'@return 
#'  Returns a partial or complete list of (actual) parameter values, as a
#'  named list.
#'
#'@seealso \code{\link{getTpPar}}.
#'
#'@export 
#'
tpPar <- function( 
    par    = NULL, 
    reset  = FALSE, 
    testRange, 
    testSum, 
    fracSumTol, 
    vertices, 
    classes, 
    scale, 
    okClock, 
    sp, 
    onFailure, 
    
    ticksAt, 
    ticksShift, 
    ticksLabelsShift, 
    arrowsShift, 
    arrowsHeight, 
    arrowsCoords, 
    arrowsBreak, 
    arrowsLength, 
    grid.line.col, 
    grid.line.lwd, 
    axis.line.lwd, 
    axis.line.col, 
    ticks.line.lwd, 
    ticks.line.col, 
    plot.bg, 
    class.label.col, 
    class.border.col, 
    class.bg, 
    class.border.lwd 
){  
    parList <- names( formals(tpPar) ) 
    parList <- parList[ !(parList %in% c( "par", "reset" )) ] 
    
    
    ## (1) Reset the parameter values:
    if( reset ){ 
        v  <- as.list( .tpParList ) 
        nv <- names( v ) 
        
        lapply( 
            X   = 1:length(v), 
            FUN = function(X){ 
                assign( x = nv[ X ], value = v[[ X ]], envir = tpParList ) 
            }   
        )   
        
        rm( nv, v ) 
    }   
    
    
    ## (2) Change the parameter values:
    
    # Get actual parameter values:
    tpParValues <- as.list( get( x = "tpParList" ) ) 
    
    # Case: par is a list of parameters to be set
    if( is.list( par ) ){
        parNames <- names( par ) 
         
        if( is.null( parNames ) ){ 
            stop( "If 'par' is a list, its item must be named." )
        }   
        
        # Check that all parameters in par exists:
        testpar1 <- !(parNames %in% names(tpParValues)) 
        
        if( any( testpar1 ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( parNames[ testpar1 ], collapse=", " ) 
            ) ) 
        }  
        
        # Set the values
        for( i in parNames ){ 
            tpParValues[[ i ]] <- par[[ i ]] 
        }   
    }   
    
    # Set all the individual parameters provided as a function's 
    # argument(s)
    for( parLabel in parList ){ 
        testExpr <- substitute( 
            expr = !missing(theLabel), 
            env  = list( theLabel = as.symbol(parLabel) ) 
        )   
        
        if( eval( testExpr ) ){ 
            tpParValues[[ parLabel ]] <- get( x = parLabel )  
        }   
    }   
    
    # Set the parameter values at once 
    nv <- names( tpParValues ) 
    lapply( 
        X   = 1:length(tpParValues), 
        FUN = function(X){ 
            assign( x = nv[ X ], value = tpParValues[[ X ]], envir = tpParList ) 
        }   
    )   
    
    
    ## (3) Return the parameter values:
    
    # Case: return the value of some parameters:
    if( is.character(par) & (length(par) != 0) ){ 
        # Test that all demanded parameters exists:    
        testpar <- !(par %in% names(tpParValues)) 
        
        if( any( testpar ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( par[ testpar ], collapse=", " ) 
            ) ) 
        }  
        
        ret <- tpParValues[ par ] 
    
    # Case: return the value of all parameters:
    }else{ 
        ret <- tpParValues 
    }   
    
    return( invisible( ret ) ) 
### Returns a partial or complete list of (actual) parameter values, 
### as a named list.
}   



# getTpPar ======================================================

#'Get a single default parameters for the package.
#'
#'Get a single default parameters for the package. Wrapper around
#'  \code{\link{tpPar}}.
#'
#'
#'@param par 
#'  See the \code{par} argument in \code{\link{tpPar}}. Notice that if
#'  more than one parameter name is provided, only the first one will be
#'  returned.
#'
#'
#'@return 
#'  Return the value of the parameter \code{par}, without the list
#'  container of \code{\link{tpPar}}.
#'
#'@export 
#'
getTpPar <- function(
    par 
){  
    return( tpPar( par = par )[[ 1L ]] ) 
}   



# ===============================================================
# Test that all parameters in '.tpParList' have been included in 
# the function rspParameters() 

# List of parameter names:
parNames <- names( as.list( .tpParList ) ) 

# List of argument names
tpParF <- names(formals(tpPar))
tpParF <- tpParF[ !(tpParF %in% c("par","reset")) ]

# List of parameters handled by tpPar(): do they match with 
# the default parameters?
testpar  <- !(parNames %in% tpParF)

if( any(testpar) ){ 
    stop( sprintf( 
        "Some parameters in '.tpParList' are not in names(formals(tpPar)): %s", 
        paste( parNames[ testpar ], collapse = ", " ) 
    ) )  
}   

# Other way round
testpar2 <- !(tpParF %in% parNames)

if( any(testpar2) ){ 
    stop( sprintf( 
        "Some parameters in names(formals(tpPar)) are not in '.tpParList': %s", 
        paste( tpParF[ testpar2 ], collapse = ", " ) 
    ) )  
}   

rm( testpar, parNames, testpar2, tpParF ) 



# Set the current list of parameters
tpParList <- list2env( as.list( .tpParList ) ) 
