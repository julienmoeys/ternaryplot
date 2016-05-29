
# +--------------------------------------------------------+
# | Language: R + roxygen2 inline documentation            |
# | Package: soiltexture 2                                 |
# | Author(s): Julien Moeys <julienmoeys@@yahoo.fr>        |
# | License: AGPL3, Affero General Public License v.3      | 
# +--------------------------------------------------------+



# ===============================================================
# Create two environment that will contain the package parameters

# - Backup / reference 

#' Environment containng a backup copy of all soiltexture2 package-options
#'
#' Environment containing a backup copy of all soiltexture2 
#'  package-options
#'
#'
#' @format 
#'  An environment, where each item corresponds to an 
#'  argument in \code{\link[soiltexture2]{stPar}}.
#'
#'
#'@rdname stParList-environment-backup
#'
#'@keywords internal
#'
.stParList <- new.env() 

# - User visible container

#' Environment containing all soiltexture2 package-options
#'
#' Environment containing all soiltexture2 package-options
#'
#'
#' @format 
#'  An environment, where each item corresponds to an 
#'  argument in \code{\link[soiltexture2]{stPar}}.
#'
#'
#'@rdname stParList-environment
#'
#'@export
#'
#'@keywords internal
#'
stParList  <- new.env() 



# Set some default parameters: 

# NON-GRAPHICAL PARAMETERS
# ========================

.stParList[[ "dummy" ]] <- "dummy argument"



# stPar =========================================================

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
## # GENERAL PARAMETERS ------------------------------------
#'
#'@param par  
#'  Three possible cases: \itemize{ \item If \code{par} is \code{NULL}
#'  (default): All the actual value of the parameters will be silently returned.
#'  \item If \code{par} is a vector of character strings representing parameter
#'  names. The value of the parameters named here will be (silently) returned.
#'  \item If \code{par} is a list following the format \code{tag = value}, where
#'  \code{tag} is the name of the parameter to be changed, and \code{value} is
#'  its new value.  Such a list is returned by \code{stPar()}. Notice that
#'  parameters can also be set individually, using the options listed below. }
#'
#'@param reset 
#'  Single logical. If TRUE, all the parameters will be set to their
#'  default value. Values are reset before any change to the parameter values, as
#'  listed below.
#'
#'@param dummy 
#'  A dummy argument
#'
#'
#'@return 
#'  Returns a partial or complete list of (actual) parameter 
#'  values, as a tagged list.
#'
#'
#'@seealso \code{\link{getStPar}}.
#'
#'@export 
#'
stPar <- function( 
    # GENERAL PARAMETERS
    par    = NULL, 
    reset  = FALSE, 
    dummy 
){  
    parList <- names( formals(stPar) ) 
    parList <- parList[ !(parList %in% c( "par", "reset" )) ] 
    
    
    ## (1) Reset the parameter values:
    if( reset ){ 
        v  <- as.list( .stParList ) 
        nv <- names( v ) 
        
        lapply( 
            X   = 1:length(v), 
            FUN = function(X){ 
                assign( x = nv[ X ], value = v[[ X ]], envir = stParList ) 
            }   
        )   
        
        rm( nv, v ) 
    }   
    
    
    ## (2) Change the parameter values:
    
    # Get actual parameter values:
    stParValues <- as.list( get( x = "stParList" ) ) 
    
    # Case: par is a list of parameters to be set
    if( is.list( par ) ){
        parNames <- names( par ) 
         
        if( is.null( parNames ) ){ 
            stop( "If 'par' is a list, its item must be named." )
        }   
        
        # Check that all parameters in par exists:
        testpar1 <- !(parNames %in% names(stParValues)) 
        
        if( any( testpar1 ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( parNames[ testpar1 ], collapse=", " ) 
            ) ) 
        }  
        
        # Set the values
        for( i in parNames ){ 
            if( is.null( par[[ i ]] ) ){
                stParValues[ i ] <- list( NULL ) 
            }else{
                stParValues[[ i ]] <- par[[ i ]] 
            }   
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
            tmpVal <- get( x = parLabel ) 
            
            if( is.null( tmpVal ) ){
                stParValues[ parLabel ] <- list( NULL ) 
                
            }else{
                stParValues[[ parLabel ]] <- tmpVal 
            };  rm( tmpVal )
        }   
    }   
    
    # Set the parameter values at once 
    nv <- names( stParValues ) 
    lapply( 
        X   = 1:length(stParValues), 
        FUN = function(X){ 
            assign( x = nv[ X ], value = stParValues[[ X ]], envir = stParList ) 
        }   
    )   
    
    
    ## (3) Return the parameter values:
    
    # Case: return the value of some parameters:
    if( is.character(par) & (length(par) != 0) ){ 
        # Test that all demanded parameters exists:    
        testpar <- !(par %in% names(stParValues)) 
        
        if( any( testpar ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( par[ testpar ], collapse=", " ) 
            ) ) 
        }  
        
        ret <- stParValues[ par ] 
    
    # Case: return the value of all parameters:
    }else{ 
        ret <- stParValues 
    }   
    
    return( invisible( ret ) ) 
### Returns a partial or complete list of (actual) parameter values, 
### as a named list.
}   



# getStPar ======================================================

#'Get a single default parameters for the package.
#'
#'Get a single default parameters for the package. Wrapper around
#'  \code{\link{stPar}}.
#'
#'
#'@param par 
#'  See the \code{par} argument in \code{\link{stPar}}. Notice that if
#'  more than one parameter name is provided, only the first one will be
#'  returned.
#'
#'
#'@return 
#'  Return the value of the parameter \code{par}, without the list
#'  container of \code{\link{stPar}}.
#'
#'@export 
#'
getStPar <- function(
    par 
){  
    return( stPar( par = par )[[ 1L ]] ) 
}   



# ===============================================================
# Test that all parameters in '.stParList' have been included in 
# the function rspParameters() 

# List of parameter names:
parNames <- names( as.list( .stParList ) ) 

# List of argument names
stParF <- names(formals(stPar))
stParF <- stParF[ !(stParF %in% c("par","reset")) ]

# List of parameters handled by stPar(): do they match with 
# the default parameters?
testpar  <- !(parNames %in% stParF)

if( any(testpar) ){ 
    stop( sprintf( 
        "Some parameters in '.stParList' are not in names(formals(stPar)): %s", 
        paste( parNames[ testpar ], collapse = ", " ) 
    ) )  
}   

# Other way round
testpar2 <- !(stParF %in% parNames)

if( any(testpar2) ){ 
    stop( sprintf( 
        "Some parameters in names(formals(stPar)) are not in '.stParList': %s", 
        paste( stParF[ testpar2 ], collapse = ", " ) 
    ) )  
}   

rm( testpar, parNames, testpar2, stParF ) 



# Set the current list of parameters
stParList <- list2env( as.list( .stParList ) ) 
