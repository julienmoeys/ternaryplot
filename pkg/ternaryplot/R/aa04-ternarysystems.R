
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# ternarySystemEnv =========================================

# New environment that will contain the definition of a few 
#   ternary classification systems
ternarySystemEnv  <- new.env() 


# Default, empty, ternary classification:
ternarySystemEnv[[ "default" ]] <- createTernarySystem()     



# Dummy ternary classification:
ternarySystemEnv[[ "dummy" ]] <- createTernarySystem( 
    "ternaryGeometry"   = createTernaryGeometry(), 
    "ternaryVariables"  = createTernaryVariables(
        "blrNames"  = c( "F1", "F2", "F3" ), 
        "blrLabels" = c( "Fraction 1 [%]", "Fraction 2 [%]", "Fraction 3 [%]" ), 
    ),  
    "main"              = "Ternary plot (dummy)", 
    "vertices"          = data.frame( 
        "id"    = c(  1,   2,   3,   4), 
        "F1"    = c(000, 000, 050, 100), 
        "F2"    = c(100, 000, 000, 000), 
        "F3"    = c(000, 100, 050, 000)  
    ),  
    "classes"           = data.frame( 
        "abbrev" = c( 
            "C1", 
            "C2" ), 
        "name" = c( 
            "Class 1", 
            "Class 2" ),  
        "verticesId" = I( list( # The length of each item can vary
            "C1" = c( 1, 2, 3 ), 
            "C2" = c( 1, 3, 4 ) 
        ) ),
        stringsAsFactors = FALSE 
    ),  
    "scale"             = NULL, 
    "over"              = NULL 
)     



# HYPRES / EU Soil Map texture triangle
ternarySystemEnv[[ "hypres" ]] <- createTernarySystem( 
    
    # Info from SysCan "FAO Soil Texture" (in fact, not an 
    # FAO soil texture!) 
    # http://sis.agr.gc.ca/cansis/nsdb/lpdb/faotext.html
    # 
    # <quote>
    # >> Texture is the relative proportion of sand, silt and 
    # >> clay of the dominant soil for each soil map polygon. 
    # >> Texture classes are:
    # 
    # >> Coarse texture: sands, loamy sand and sandy loams 
    # >> with less than 18 % clay, and more than 65 % sand.
    #
    # >> Medium texture: sandy loams, loams, sandy clay loams, 
    # >> silt loams with less than 35 % clay and less than 
    # >> 65 % sand; the sand fractions may be as high as 82 % 
    # >> if a minimum of 18 % clay is present.
    #
    # >> Fine texture: clays, silty clays, sandy clays, clay 
    # >> loams and silty clay loams with more than 35 % clay.
    #
    # >> Where two or three texture names appear, this means 
    # >> that all named textures are present in the map unit.
    # 
    # >> Texture Codeset
    # >> COARSE
    # >> FINE
    # >> FINE-COARSE
    # >> FINE-MED-CRS
    # >> FINE-MEDIUM
    # >> MEDIUM
    # >> MEDIUM-COARSE
    # </quote>
    
    "ternaryVariables" = createTernaryVariables( 
        "blrNames"  = c( "SAND", "CLAY", "SILT" ), 
        "blrLabels" = c(
            "Sand 0.05 - 2 mm [%]", 
            "Clay 0 - 0.002 mm [%]", 
            "Silt 0.002 - 0.05 mm [%]" 
        )   
        
        # # Expressions do not follow par( 'font' )
        # expression( 
            # 'Sand 50-2000' ~ mu * 'm [%]', 
            # 'Clay 0-2' ~ mu * 'm [%]', 
            # 'Silt 2-50' ~ mu * 'm [%]'
        # )   
    ),  
    
    "ternaryGeometry" = createTernaryGeometry( 
        "tlrAngles" = c( 60, 60, 60 ), 
        "blrClock"  = rep( TRUE, 3 ), 
        "fracSum"   = 100 
    ), 
    
    "main" = "HYPRES / EU Soil Map texture triangle", 
    
    "vertices" = data.frame( 
        "id"    = c(   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12 ), 
        "CLAY"  = c( 100, 060, 060, 035, 035, 035, 018, 018, 000, 000, 000, 000 ), 
        "SILT"  = c( 000, 000, 040, 000, 050, 065, 000, 017, 000, 035, 085, 100 ), 
        "SAND"  = c( 000, 040, 000, 065, 015, 000, 082, 065, 100, 065, 015, 000 )   
    ),  
    
    "classes" = data.frame( 
        abbrev = c( 
            "C", 
            "MF", 
            "M", 
            "F", 
            "VF" ), 
        name   = c(
            "Coarse", 
            "Medium fine", 
            "Medium", 
            "Fine", 
            "Very fine" ), 
        verticesId = I( list( 
            C  = c(9, 7, 8, 10), 
            MF = c(11, 5, 6, 12), 
            M  = c(7, 4, 5, 11, 10, 8), 
            F  = c(4, 2, 3, 6), 
            VF = c(2, 1, 3) ) ), 
        stringsAsFactors = FALSE 
    ),  
    
    "scale" = NULL, 
    "over"  = NULL 
)   



# Dummy ternary classification (2):
ternarySystemEnv[[ "dummy2" ]] <- createTernarySystem( 
    "ternaryGeometry"   = createTernaryGeometry(
        tlrAngles = c( 45, 90, 45 ), 
        blrClock  = c( FALSE, TRUE, NA )
    ),  
    "ternaryVariables"  = createTernaryVariables(
        "blrNames"  = c( "F1", "F2", "F3" ), 
        "blrLabels" = c( "Fraction 1 [%]", "Fraction 2 [%]", "Fraction 3 [%]" ), 
    ),  
    "main"              = "Ternary plot (dummy)", 
    "vertices"          = data.frame( 
        "id"    = c(  1,   2,   3,   4), 
        "F1"    = c(000, 000, 050, 100), 
        "F2"    = c(100, 000, 050, 000), 
        "F3"    = c(000, 100, 000, 000)  
    ),  
    "classes"           = data.frame( 
        "abbrev" = c( 
            "D1", 
            "D2" ), 
        "name" = c( 
            "Class 1", 
            "Class 2" ),  
        "verticesId" = I( list( # The length of each item can vary
            "D1" = c( 1, 2, 3 ), 
            "D2" = c( 2, 4, 3 ) 
        ) ),
        stringsAsFactors = FALSE 
    ),  
    "scale"             = NULL, 
    "over"              = NULL 
)     



# Dummy ternary classification (3):
ternarySystemEnv[[ "dummy3" ]] <- createTernarySystem( 
    "ternaryGeometry"   = createTernaryGeometry(),  
    "ternaryVariables"  = createTernaryVariables(
        "blrNames"  = c( "F1", "F2", "F3" ), 
        "blrLabels" = c( "Fraction 1 [%]", "Fraction 2 [%]", "Fraction 3 [%]" ), 
    ),  
    "main"              = "Ternary plot (dummy)", 
    "vertices"          = data.frame( 
        "id"    = c(  1,   2,   3,   4), 
        "F1"    = c(000, 000, 050, 100), 
        "F2"    = c(100, 000, 050, 000), 
        "F3"    = c(000, 100, 000, 000)  
    ),  
    "classes"           = data.frame( 
        "abbrev" = c( 
            "D1", 
            "D2" ), 
        "name" = c( 
            "Class 1", 
            "Class 2" ),  
        "verticesId" = I( list( # The length of each item can vary
            "D1" = c( 1, 2, 3 ), 
            "D2" = c( 2, 4, 3 ) 
        ) ),
        stringsAsFactors = FALSE 
    ),  
    "scale"             = NULL, 
    "over"              = NULL 
)     



# getAllTernarySystems =====================================

#'INTERNAL: Fetch all the pre-defined ternary classification system
#'
#'INTERNAL: Fetch all the pre-defined ternary classification 
#'  system, as a list of 
#'  \code{\link[ternaryplot]{ternarySystem-class}}es.
#'
#'  The function is designed for internal use, in combination 
#'  with the argument \code{terSysEnvList} of the functions 
#'  \code{link[ternaryPlot]{tpPar}} or 
#'  \code{link[ternaryPlot]{getTpPar}}, as a flexible 
#'  mechanism to manage the list of 
#'  \code{\link[ternaryplot]{ternarySystem-class}} objects 
#'  used by the package.
#'
#'
#'@return 
#'  A \code{\link[base]{list}} 
#'  \code{\link[ternaryplot]{ternarySystem-class}} objects.
#'
#'
#'@export 
#'
#'@keywords internal 
#'
getAllTernarySystems <- function(){    
    return( as.list( ternarySystemEnv ) ) 
}   



# getTernarySystem =========================================

#'Fetch a pre-defined ternary classification system
#'
#'Fetch a pre-defined ternary classification system
#'
#'
#'@seealso \code{\link[ternaryplot]{listTernarySystem}}, 
#'  \code{\link[ternaryplot]{addTernarySystem}} and 
#'  \code{\link[ternaryplot]{deleteTernarySystem}}.
#'
#'
#'@param s 
#'  Single character string. Name of the ternary classification to 
#'  be fetched.
#'
#'
#'@return 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object.
#'
#'
#'@example inst/examples/getTernarySystem-example.R
#'
#'@export 
#'
getTernarySystem <- function( s = "default" ){    
    if( !is.character( s ) ){ 
        stop( "'s' must be a character string" )
    }   
    
    if( length( s ) > 1L ){ 
        stop( sprintf( 
            "'s' must be a single character string (now length %s)", 
            length( s ) 
        ) ) 
    }   
    
    .tpPar <- tpPar()
    terSysEnvList <- .tpPar[[ "terSysEnvList" ]]
    
    lts <- listTernarySystem( definition = TRUE )
    
    if( any( s %in% lts[, "systemName" ] ) ){
        sel <- which( lts[, "systemName" ] == s )[ 1L ]
        out <- lts[ sel, "ternarySystem" ][[ 1L ]] 
        
    }else{
        stop( sprintf( 
            "The ternary system (%s) could not be found", 
            s 
        ) )  
    }   
      
    return( out ) 
}   



# listTernarySystem ========================================

#'List all pre-defined ternary classification systems
#'
#'List all pre-defined ternary classification systems
#'
#'
#'@seealso \code{\link[ternaryplot]{getTernarySystem}}, 
#'  \code{\link[ternaryplot]{addTernarySystem}} and 
#'  \code{\link[ternaryplot]{deleteTernarySystem}}.
#'
#'
#'@param definition 
#'  Single logical value. If \code{TRUE} (not the default), 
#'  the definition of the \code{\link[ternaryplot]{ternarySystem-class}} 
#'  objects are exported too (in a column named 
#'  \code{ternarySystem})
#'
#'
#'@return 
#'  A \code{\link[base]{data.frame}}, with the following 
#'  columns: \code{sourceIndex}, \code{sourceName} (the 
#'  index and the name of the source as listed in 
#'  \code{getTpPar('terSysEnvList')}), \code{systemName} 
#'  (the name of the ternary system) and optionally 
#'  \code{ternarySystem} (the full definition of the 
#'  ternary system, if \code{definition=TRUE}).
#'
#'
#'@example inst/examples/listTernarySystem-example.R
#'
#'@export 
#'
listTernarySystem <- function( definition = FALSE ){ 
    .tpPar        <- tpPar()
    terSysEnvList <- .tpPar[[ "terSysEnvList" ]]
    
    nm <- names( terSysEnvList ) 
    
    if( is.null( nm ) ){
        names( terSysEnvList ) <- as.character( 1:length(terSysEnvList) ) 
        nm <- names( terSysEnvList ) 
    }else{
        if( any( dup <- duplicated( nm ) ) ){
            warning( sprintf(
                "Some names in names(getTpPar('terSysEnvList')) are duplicated: %s", 
                paste( x = nm[ dup ], collapse = "; " ) 
            ) ) 
        };  rm( dup ) 
    }   
    
    tsList <- lapply(
        X   = 1:length(terSysEnvList), 
        FUN = function(i){
            terSysEnvList0 <- terSysEnvList[[ i ]]()
            
            if( !( "list" %in% class( terSysEnvList0 ) ) ){
                stop( sprintf(
                    "getTpPar('terSysEnvList')[[%s]]() should return a 'list'. Now: %s", 
                    i, paste( class( terSysEnvList0 ), collapse = "; " ) 
                ) ) 
            }   
            
            nm0 <- names( terSysEnvList0 )
            
            if( is.null( nm0 ) ){
                stop( sprintf(
                    "names(getTpPar('terSysEnvList')[[%s]]()) is NULL (the list should be tagged/named)", 
                    i 
                ) ) 
            }   
            
            out <- data.frame(
                "sourceIndex"    = i, 
                "sourceName"     = nm[ i ], 
                "systemName"     = nm0, 
                stringsAsFactors = FALSE 
            )   
            
            if( definition ){
                out <- data.frame(
                    out, 
                    "ternarySystem" = I( terSysEnvList0 ), 
                    stringsAsFactors = FALSE 
                )   
            }   
            
            return( out )
        }   
    )   
    
    tsList <- do.call( what = "rbind", args = tsList ) 
    
    return( tsList ) 
}   



# addTernarySystem =========================================

#'Add or update a ternary system to the list of available ternary systems (only for this session)
#'
#'Add or update a ternary system to the list of available 
#'  ternary systems. Only valid for that R session and until 
#'  it is closed (i.e. not permanent effects). It only adds 
#'  the ternary system for the ternary-plot package.
#'
#'
#'@seealso \code{\link[ternaryplot]{getTernarySystem}}, 
#'  \code{\link[ternaryplot]{listTernarySystem}} and 
#'  \code{\link[ternaryplot]{deleteTernarySystem}}.
#'  \code{\link[ternaryplot]{createTernarySystem}} should 
#'  be used to create/define a ternary system before adding 
#'  it to the list of available ternary systems. 
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem-class}} object, 
#'  such as obtained with 
#'  \code{\link[ternaryplot]{getTernarySystem}}.
#'
#'@param name 
#'  Single character string. Name (label) of the ternary 
#'  system to be added.
#'
#'@param overwrite 
#'  Single logical value. If \code{TRUE} (not the default), 
#'  existing ternary system with the same name as in 
#'  \code{name} will be overwritten.
#'
#'
#'@return 
#'  Nothing. Used for its side effect.
#'
#'
#'@example inst/examples/addTernarySystem-example.R
#'
#'@rdname addTernarySystem-methods
#'
#'@export 
#'
addTernarySystem <- function( s, name, overwrite = FALSE ){ 
    UseMethod( "addTernarySystem" )
}   


#'@rdname addTernarySystem-methods
#'
#'@method addTernarySystem ternarySystem
#'
#'@export
#'
addTernarySystem.ternarySystem <- function( s, name, overwrite = FALSE ){    
    ternaryCheck( s = s ) 
    
    if( missing( "name" ) ){
        stop( "'name' is missing, with no default." )
    }   
    
    if( length( name ) > 1 ){
        stop( "length( name ) > 1. Only one name should be provided." )
    }   
    
    # Get all the ternary classifications of the package:
    ternarySystemE <- as.list( ternarySystemEnv ) 
    tsList <- names( ternarySystemE )
    
    if( (name %in% tsList) & (!overwrite) ){
        stop( sprintf( 
            "'name' (%s) already exists in the list of ternary systems (ternaryplot-package), while overwrite is FALSE.", 
            name 
        ) ) 
    }   
    
    assign( 
        x     = name, 
        value = s, 
        envir = ternarySystemEnv )
    
    return( invisible( TRUE ) )
}   


# deleteTernarySystem ======================================

#'Delete a ternary system from the list of available ternary systems (only for this session)
#'
#'Delete a ternary system from the list of available ternary 
#'  systems. Only valid for that R session (unless the 
#'  ternarySystem had been added during the session using 
#'  \code{\link[ternaryplot]{addTernarySystem}}, in which 
#'  case it disappears anyway at the end of the session). 
#'  Can only delete a ternary system for the ternary-plot 
#'  package.
#'
#'
#'@seealso \code{\link[ternaryplot]{getTernarySystem}}, 
#'  \code{\link[ternaryplot]{listTernarySystem}} and 
#'  \code{\link[ternaryplot]{addTernarySystem}}.
#'
#'
#'@param name 
#'  Vector of character strings. Name of the ternary systems 
#'  to be deleted from the list of available systems.
#'
#'
#'@return 
#'  Nothing. Used for its side effect.
#'
#'
#'@example inst/examples/deleteTernarySystem-example.R
#'
#'@export 
#'
deleteTernarySystem <- function( name ){ 
    if( missing( "name" ) ){
        stop( "'name' is missing, with no default." )
    }   

    if( !is.character( name ) ){
        stop( "'name' must be a vector of character strings" ) 
    }   
    
    # Get all the ternary classifications of the package:
    ternarySystemE <- as.list( ternarySystemEnv ) 
    tsList <- names( ternarySystemE )
    
    if( any( nNotInList <- !(name %in% tsList) ) ){
        stop( sprintf( 
            "Some value(s) in 'name' cannot be found in the list of ternary systems (ternaryplot-package): %s. See also listTernarySystem()", 
            paste( name[ nNotInList ], collapse = "; " ) 
        ) ) 
    }   
    
    rm( list = name, envir = ternarySystemEnv )
    
    return( invisible( TRUE ) )
}   

