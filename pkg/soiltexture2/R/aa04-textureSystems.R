
# +--------------------------------------------------------+
# | Language: R + roxygen2 inline documentation            |
# | Package: soiltexture 2                                 |
# | Author(s): Julien Moeys <julienmoeys@@yahoo.fr>        |
# | License: AGPL3, Affero General Public License v.3      | 
# +--------------------------------------------------------+



# soilTextEnvList ==========================================

# New environment that will contain the definition of a few 
#   ternary classification systems
soilTextEnvList  <- new.env() 


# Default, empty, ternary classification:
soilTextEnvList[[ "default" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        "ternaryVariables" = createTernaryVariables( 
            "blrNames"  = c( "SAND", "CLAY", "SILT" ), 
            "blrLabels" = c(
                "Sand 0.05 - 2 mm [%]", 
                "Clay 0 - 0.002 mm [%]", 
                "Silt 0.002 - 0.05 mm [%]" 
            ) ),  
        "ternaryGeometry" = createTernaryGeometry( ##
            "fracSum"   = 100, 
            "tlrAngles" = c( 60, 60, 60 ), # top, left, right angles
            "blrClock"  = c( TRUE, TRUE, TRUE )
        ),  
        "main" = character(0) ), 
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        "SAND" = 2000 ) 
)   



# HYPRES / EU Soil Map texture triangle
soilTextEnvList[[ "hypres" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        
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
            "abbrev" = c( "C", "M", "MF", "F", "VF" ), 
            "name" = c( 
                "Coarse", 
                "Medium", 
                "Medium fine", 
                "Fine", 
                "Very fine" ), 
            "verticesId" = I( list( 
                "C"  = c(09,07,08,10), 
                "M"  = c(07,04,05,11,10,08), 
                "MF" = c(11,05,06,12), 
                "F"  = c(04,02,03,06), 
                "VF" = c(02,01,03) 
            ) ), 
            stringsAsFactors = FALSE 
        ),  
        
        "scale" = NULL, 
        "over"  = NULL 
    ),  
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        "SAND" = 2000 ) 
)   



# getAllTextureSystems =====================================

#'INTERNAL: Fetch all the pre-defined texture classification systems
#'
#'INTERNAL: Fetch all the pre-defined texture classification 
#'  systems, as a list of 
#'  \code{\link[soiltexture2]{textureSystem-class}}es.
#'
#'  The function is designed for internal use, in combination 
#'  with the argument \code{terSysEnvList} of the functions 
#'  \code{link[soiltexture2]{stPar}} or 
#'  \code{link[soiltexture2]{getStPar}}, as a flexible 
#'  mechanism to manage the list of 
#'  \code{\link[soiltexture2]{textureSystem-class}} objects 
#'  used by the package.
#'
#'
#'@return 
#'  A \code{\link[base]{list}} 
#'  \code{\link[soiltexture2]{textureSystem-class}} objects.
#'
#'
#'@export 
#'
#'@keywords internal 
#'
getAllTextureSystems <- function(){    
    return( as.list( soilTextEnvList ) ) 
}   

