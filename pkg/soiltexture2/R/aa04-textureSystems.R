
# +--------------------------------------------------------+
# | Language: R + roxygen2 inline documentation            |
# | Package: soiltexture 2                                 |
# | Author(s): Julien Moeys <julienmoeys@@yahoo.fr>        |
# | License: AGPL3, Affero General Public License v.3      | 
# +--------------------------------------------------------+



# LIST OF SPECIAL CHARACTERS
# See charToRaw("é")
# *   é -> \xe9

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
# ----------------------------------------------------------

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
        ),  
        
        "ternaryGeometry" = createTernaryGeometry( 
            "tlrAngles" = c( 60, 60, 60 ), 
            "blrClock"  = rep( TRUE, 3 ), 
            "fracSum"   = 100 
        ), 
        
        "main" = "HYPRES / EU Soil Map soil texture classification", 
        
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



# USDA Triangle parameters
# ----------------------------------------------------------

soilTextEnvList[[ "usda" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        
        "ternaryVariables" = createTernaryVariables( 
            "blrNames"  = c( "SAND", "CLAY", "SILT" ), 
            "blrLabels" = c(
                "Sand 0.05 - 2 mm [%]", 
                "Clay 0 - 0.002 mm [%]", 
                "Silt 0.002 - 0.05 mm [%]" 
            )   
        ),  
        
        "ternaryGeometry" = createTernaryGeometry( 
            "tlrAngles" = c( 60, 60, 60 ), 
            "blrClock"  = rep( TRUE, 3 ), 
            "fracSum"   = 100 
        ), 
        
        "main" = "USDA soil texture classification", 
        
        "vertices" = data.frame( 
            "id"        = c( 1,     2,     3,     4,     5,     6,     7,     8,     9,     10,    11,    12,    
                             13,    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,    
                             24,    25,    26 ), 
            "CLAY"      = c( 0.550, 0.600, 0.350, 0.350, 0.400, 0.400, 0.400, 0.200, 0.200, 0.275, 0.275, 0.275,  
                             0.275, 0.150, 0.100, 0.075, 0.075, 0.125, 0.125, 0.000, 0.000, 0.000, 0.000,         
                             1.000, 0.000, 0.000  ) * 100,  
                        #
            "SILT"      = c( 0.000, 0.400, 0.000, 0.200, 0.150, 0.400, 0.600, 0.000, 0.275, 0.275, 0.500, 0.525,  
                             0.725, 0.000, 0.000, 0.400, 0.500, 0.800, 0.875, 0.150, 0.300, 0.500, 0.800,         
                             0.000, 0.000, 1.000  ) * 100,  
                        #
            "SAND"      = c( 0.450, 0.000, 0.650, 0.450, 0.450, 0.200, 0.000, 0.800, 0.525, 0.450, 0.225, 0.200,  
                             0.000, 0.850, 0.900, 0.525, 0.425, 0.075, 0.000, 0.850, 0.700, 0.500, 0.200,         
                             0.000, 1.000, 0.000  ) * 100  
        ),  
        
        "classes" = data.frame( 
            "abbrev" = c(
                "Cl", 
                "SiCl", 
                "SaCl", 
                "ClLo", 
                "SiClLo", 
                "SaClLo", 
                "Lo", 
                "SiLo", 
                "SaLo", 
                "Si", 
                "LoSa", 
                "Sa" ), 
            "name" = c(
                "clay", 
                "silty clay", 
                "sandy clay", 
                "clay loam", 
                "silty clay loam", 
                "sandy clay loam", 
                "loam", 
                "silty loam", 
                "sandy loam", 
                "silt", 
                "loamy sand", 
                "sand"
            ),  
            "verticesId" = I( list(
                "Cl"     = c(24, 1, 5, 6, 2), 
                "SiCl"   = c(2, 6, 7), 
                "SaCl"   = c(1, 3, 4, 5), 
                "ClLo"   = c(5, 4, 10, 11, 12, 6), 
                "SiClLo" = c(6, 12, 13, 7), 
                "SaClLo" = c(3, 8, 9, 10, 4), 
                "Lo"     = c(10, 9, 16, 17, 11), 
                "SiLo"   = c(11, 17, 22, 23, 18, 19, 13, 12), 
                "SaLo"   = c(8, 14, 21, 22, 17, 16, 9), 
                "Si"     = c(18, 23, 26, 19), 
                "LoSa"   = c(14, 15, 20, 21), 
                "Sa"     = c(15, 25, 20) 
    ) ), 
            stringsAsFactors = FALSE 
        ),  
        
        "scale" = NULL, 
        "over"  = NULL 
    ),  
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        "SAND" = 2000 ) 
)   



# AISNE SOIL TEXTURE CLASSIFICATION (FRANCE)
# ----------------------------------------------------------

soilTextEnvList[[ "aisne" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        
        "ternaryVariables" = createTernaryVariables( 
            "blrNames"  = c( "SAND", "CLAY", "SILT" ),
            "blrLabels" = c(  
                "Sand 0.05 - 2 mm [%]", 
                "Clay 0 - 0.002 mm [%]", 
                "Silt 0.002 - 0.05 mm [%]" 
            )   
        ),  
        
        "ternaryGeometry" = createTernaryGeometry( 
            "tlrAngles" = c( 60, 60, 60 ), 
            "blrClock"  = rep( TRUE, 3 ), 
            "fracSum"   = 100 
        ), 
        
        "main" = "Aisne soil texture classification (fr)", 
        
        "vertices" = data.frame( 
            "id"        = c( 1,     2,     3,     4,     5,     6,     7,     8,     9,     10,    11,    
                             12,    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,    
                             23,    24,    25,    26,    27,    28,    29 ), 
            "CLAY"      = c( 0.450, 0.450, 0.450, 0.450, 0.250, 0.250, 0.300, 0.300, 0.300, 0.300, 0.100, 
                             0.100, 0.125, 0.125, 0.175, 0.175, 0.175, 0.175, 0.000, 0.000, 0.075, 0.075, 
                             0.075, 0.075, 0.000, 0.000, 0.000, 1.000, 0.300  ) * 100,   
            "SILT"      = c( 0.000, 0.100, 0.350, 0.550, 0.000, 0.200, 0.250, 0.350, 0.500, 0.700, 0.000, 
                             0.100, 0.100, 0.325, 0.275, 0.475, 0.675, 0.825, 0.000, 0.200, 0.375, 0.575, 
                             0.775, 0.925, 0.450, 0.850, 1.000, 0.000, 0.550  ) * 100,   
            "SAND"      = c( 0.550, 0.450, 0.200, 0.000, 0.750, 0.550, 0.450, 0.350, 0.200, 0.000, 0.900, 
                             0.800, 0.775, 0.550, 0.550, 0.350, 0.150, 0.000, 1.000, 0.800, 0.550, 0.350, 
                             0.150, 0.000, 0.550, 0.150, 0.000, 0.000, 0.150  ) * 100   
        ),  
        
        "classes" = data.frame( 
            "abbrev" = c(
                "ALO", 
                "A", 
                "AL", 
                "AS", 
                "LA", 
                "LAS", 
                "LSA", 
                "SA", 
                "LM", 
                "LMS", 
                "LS", 
                "SL", 
                "S", 
                "LL", 
                "LLS"
            ),  
            "name" = c(
                "Argile lourde", 
                "Argile", 
                "Argile limoneuse", 
                "Argile sableuse", 
                "Limon argileux", 
                "Limon argilo-sableux", 
                "Limon sablo-argileux", 
                "Sable argileux", 
                "Limon moyen", 
                "Limon moyen sableux", 
                "Limon sableux", 
                "Sable limoneux", 
                "Sable", 
                "Limon l\xe9ger", 
                "Limon l\xe9ger sableux"
            ),  
            "verticesId" = I( list(
                "ALO" = c(28, 1, 4), 
                "A"   = c(2, 7, 9, 3), 
                "AL"  = c(3, 9, 10, 4), 
                "AS"  = c(1, 5, 6, 7, 2), 
                "LA"  = c(29, 17, 18, 10), 
                "LAS" = c(8, 16, 17, 29, 9), 
                "LSA" = c(7, 6, 15, 16, 8), 
                "SA"  = c(5, 11, 12, 13, 14, 15, 6), 
                "LM"  = c(17, 23, 24, 18), 
                "LMS" = c(16, 22, 23, 17), 
                "LS"  = c(15, 14, 21, 22, 16), 
                "SL"  = c(13, 12, 20, 25, 21, 14), 
                "S"   = c(11, 19, 20, 12), 
                "LL"  = c(23, 26, 27, 24), 
                "LLS" = c(21, 25, 26, 23, 22 ) 
            ) ),    
            stringsAsFactors = FALSE 
        ),  
        
        "scale" = NULL, 
        "over"  = NULL 
    ),  
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        "SAND" = 2000 ) 
)   



# GEPPA SOIL TEXTURE CLASSIFICATION (FRANCE)
# ----------------------------------------------------------

soilTextEnvList[[ "geppa" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        
        "ternaryVariables" = createTernaryVariables( 
            "blrNames"  = c( "SILT", "CLAY", "SAND" ), 
            "blrLabels" = c(  
                "Silt 0.002 - 0.05 mm [%]", 
                "Clay 0 - 0.002 mm [%]", 
                "Sand 0.05 - 2 mm [%]" 
            )   
        ),  
        
        "ternaryGeometry" = createTernaryGeometry( 
            "tlrAngles" = c(45,90,45), 
            "blrClock"  = c(F,T,NA), 
            "fracSum"   = 100 
        ), 
        
        "main" = "GEPPA soil texture classification (fr)", 
        
        "vertices" = data.frame( 
            "id"    =   c( 1,           2,           3,           4,           5,           6,           7, 
                           8,           9,           10,          11,          12,          13,          14, 
                           15,          16,          17,          18,          19,          20,          21,    
                           22,          23,          24,          25,          26,          27,          28 ),        
            "CLAY"  =   c( 1.000000000, 0.600000000, 0.550000000, 0.450000000, 0.426000000, 0.394160600, 0.375000000, 
                           0.325000000, 0.307758600, 0.287600800, 0.275000000, 0.225000000, 0.209848500, 0.203698200, 
                           0.187764100, 0.175000000, 0.125000000, 0.111486500, 0.103378400, 0.087890630, 0.075000000, 
                           0.075000000, 0.033333330, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000 ) * 100,  
            "SILT"  =   c( 0.000000000, 0.000000000, 0.450000000, 0.000000000, 0.200000000, 0.465328500, 0.625000000, 
                           0.000000000, 0.250000000, 0.542288300, 0.725000000, 0.000000000, 0.250000000, 0.351479300, 
                           0.614392600, 0.825000000, 0.000000000, 0.250000000, 0.400000000, 0.686523440, 0.925000000, 
                           0.000000000, 0.250000000, 0.000000000, 0.250000000, 0.450000000, 0.750000000, 1.000000000 ) * 100,  
            "SAND"  =   c( 0.000000000, 0.400000000, 0.000000000, 0.550000000, 0.374000000, 0.140510900, 0.000000000, 
                           0.675000000, 0.442241400, 0.170110900, 0.000000000, 0.775000000, 0.540151500, 0.444822500, 
                           0.197843300, 0.000000000, 0.875000000, 0.638513500, 0.496621600, 0.225585930, 0.000000000, 
                           0.925000000, 0.716666670, 1.000000000, 0.750000000, 0.550000000, 0.250000000, 0.000000000 ) * 100  
        ),  
        
        "classes" = data.frame( 
            "abbrev" = c(
                "AA", 
                "A", 
                "As", 
                "Als", 
                "Al", 
                "AS", 
                "LAS", 
                "La", 
                "Sa", 
                "Sal", 
                "Lsa", 
                "L", 
                "S", 
                "SS", 
                "Sl", 
                "Ls", 
                "LL"
            ),  
            "name" = c( 
                "Argile lourde", 
                "Argileux", 
                "Argile sableuse", 
                "Argile limono-sableuse", 
                "Argile limoneuse", 
                "Argilo-sableux", 
                "Limon argilo-sableux", 
                "Limon argileux", 
                "Sable argileux", 
                "Sable argilo-limoneux", 
                "Limon sablo-argileux", 
                "Limon", 
                "Sableux", 
                "Sable", 
                "Sable limoneux", 
                "Limon sableux", 
                "Limon pur"
            ),  
            "verticesId" = I( list(
                "AA"  = c(1, 2, 3), 
                "A"   = c(2, 4, 5, 6, 7, 3), 
                "As"  = c(4, 8, 9, 5), 
                "Als" = c(5, 9, 10, 6), 
                "Al"  = c(6, 10, 11, 7), 
                "AS"  = c(8, 12, 13, 9), 
                "LAS" = c(9, 13, 14, 15, 10), 
                "La"  = c(10, 15, 16, 11), 
                "Sa"  = c(12, 17, 18, 13 ), 
                "Sal" = c(13, 18, 19, 14), 
                "Lsa" = c(14, 19, 20, 15), 
                "L"   = c(15, 20, 21, 16), 
                "S"   = c(17, 22, 23, 18), 
                "SS"  = c(22, 24, 25, 23), 
                "Sl"  = c(18, 23, 25, 26, 19), 
                "Ls"  = c(19, 26, 27, 20), 
                "LL"  = c(20, 27, 28, 21 )
            ) ), 
            stringsAsFactors = FALSE 
        ),  
        
        "scale" = NULL, 
        "over"  = NULL 
    ),  
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        "SAND" = 2000 ) 
)   



# GERMAN SOIL TEXTURE CLASSIFICATION
# ----------------------------------------------------------

soilTextEnvList[[ "bk94" ]] <- createTextureSystem( 
    ternarySystem = createTernarySystem( 
        
        "ternaryVariables" = createTernaryVariables( 
            "blrNames"  = c( "CLAY", "SILT", "SAND" ), 
            "blrLabels" = c(  
                "Clay 0 - 0.002 mm [%]", 
                "Silt 0.002 - 0.05 mm [%]", 
                "Sand 0.05 - 2 mm [%]" 
            )   
        ),  
        
        "ternaryGeometry" = createTernaryGeometry( 
            "tlrAngles" = c(45,90,45), 
            "blrClock"  = c(F,T,NA), 
            "fracSum"   = 100 
        ), 
        
        "main" = "Bodenkundliche Kartieranleitung 1994 (de)", 
        
        "vertices" = data.frame( 
            "id"    =   c( 1,     2,     3,     4,     5,     6,     7,     8,     9,     10,    11,    12,    13,    
                           14,    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,    
                           27,    28,    29,    30,    31,    32,    33,    34,    35,    36,    37,    38,    39,    
                           40,    41,    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,    52, 
                           53 ), 
            "CLAY"  =   c( 0.000, 0.080, 0.120, 0.170, 0.000, 0.080, 0.250, 0.080, 0.120, 0.170, 0.250, 0.300, 0.350, 
                           0.450, 0.000, 0.080, 0.170, 0.250, 0.300, 0.350, 0.450, 0.000, 0.080, 0.120, 0.170, 0.250, 
                           0.650, 0.170, 0.250, 0.350, 0.450, 0.650, 0.000, 0.050, 0.080, 0.170, 0.250, 0.350, 0.450, 
                           0.650, 0.000, 0.050, 0.080, 0.120, 0.170, 0.000, 0.050, 0.170, 0.250, 0.350, 0.450, 0.650, 
                           1.000 ) * 100,  
            "SILT"  =   c( 1.000, 0.920, 0.880, 0.830, 0.800, 0.800, 0.750, 0.650, 0.650, 0.650, 0.650, 0.650, 0.650, 
                           0.550, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.400, 0.400, 0.400, 0.400, 0.400, 
                           0.350, 0.300, 0.300, 0.300, 0.300, 0.300, 0.250, 0.250, 0.250, 0.150, 0.150, 0.150, 0.150, 
                           0.150, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 
                           0.000 ) * 100,  
            "SAND"  =   c( 0.000, 0.000, 0.000, 0.000, 0.200, 0.120, 0.000, 0.270, 0.230, 0.180, 0.100, 0.050, 0.000, 
                           0.000, 0.500, 0.420, 0.330, 0.250, 0.200, 0.150, 0.050, 0.600, 0.520, 0.480, 0.430, 0.350, 
                           0.000, 0.530, 0.450, 0.350, 0.250, 0.050, 0.750, 0.700, 0.670, 0.680, 0.600, 0.500, 0.400, 
                           0.200, 0.900, 0.850, 0.820, 0.780, 0.730, 1.000, 0.950, 0.830, 0.750, 0.650, 0.550, 0.350, 
                           0.000 ) * 100  
        ),  
        
        "classes" = data.frame( 
            "abbrev" = c( 
                "Ss", 
                "Su2", 
                "Sl2", 
                "Sl3", 
                "St2", 
                "Su3", 
                "Su4", 
                "Slu", 
                "Sl4", 
                "St3", 
                "Ls2", 
                "Ls3", 
                "Ls4", 
                "Lt2", 
                "Lts", 
                "Ts4", 
                "Ts3", 
                "Uu", 
                "Us", 
                "Ut2", 
                "Ut3", 
                "Uls", 
                "Ut4", 
                "Lu", 
                "Lt3", 
                "Tu3", 
                "Tu4", 
                "Ts2", 
                "Tl", 
                "Tu2", 
                "Tt"
            ),  
            "name" = c(
                "reiner Sand", 
                "Schwach schluffiger Sand", 
                "Schwach lehmiger Sand", 
                "Mittel lehmiger Sand", 
                "Schwach toniger Sand", 
                "Mittel schluffiger Sand", 
                "Stark schluffiger Sand", 
                "Schluffig-lehmiger Sand", 
                "Stark lehmiger Sand", 
                "Mittel toniger Sand", 
                "Schwach sandiger Lehm", 
                "Mittel sandiger Lehm", 
                "Stark sandiger Lehm", 
                "Schwach toniger Lehm", 
                "Sandig-toniger Lehm", 
                "Stark sandiger Ton", 
                "Mittel sandiger Ton", 
                "Reiner Schluff", 
                "Sandiger Schluff", 
                "Schwach toniger Schluff", 
                "Mittel toniger Schluff", 
                "Sandig-lehmiger Schluff", 
                "Stark toniger Schluff", 
                "Schluffiger Lehm", 
                "Mittel toniger Lehm", 
                "Mittel schluffiger Ton", 
                "Stark schluffiger Ton", 
                "Schwach sandiger Ton", 
                "Lehmiger Ton", 
                "Schwach schluffiger Ton", 
                "Reiner Ton"
            ),   
            "verticesId" = I( list(
                "Ss"  = c(41, 46, 47, 42), 
                "Su2" = c(33, 41, 42, 34), 
                "Sl2" = c(34, 42, 43, 35), 
                "Sl3" = c(23, 35, 43, 44, 24), 
                "St2" = c(42, 47, 48, 45, 44, 43), 
                "Su3" = c(22, 33, 34, 35, 23), 
                "Su4" = c(15, 22, 23, 16), 
                "Slu" = c(16, 23, 24, 25, 17), 
                "Sl4" = c(24, 44, 45, 36, 28, 25), 
                "St3" = c(36, 45, 48, 49, 37), 
                "Ls2" = c(17, 25, 26, 18), 
                "Ls3" = c(25, 28, 29, 26), 
                "Ls4" = c(28, 36, 37, 29), 
                "Lt2" = c(18, 26, 29, 30, 20, 19), 
                "Lts" = c(29, 37, 38, 39, 31, 30), 
                "Ts4" = c(37, 49, 50, 38), 
                "Ts3" = c(38, 50, 51, 39), 
                "Uu"  = c(1, 5, 6, 2), 
                "Us"  = c(5, 15, 16, 8, 6), 
                "Ut2" = c(2, 6, 8, 9, 3), 
                "Ut3" = c(3, 9, 10, 4), 
                "Uls" = c(8, 16, 17, 10, 9), 
                "Ut4" = c(4, 10, 11, 7), 
                "Lu"  = c(10, 17, 18, 19, 12, 11), 
                "Lt3" = c(20, 30, 31, 21), 
                "Tu3" = c(12, 19, 20, 21, 14, 13), 
                "Tu4" = c(7, 11, 12, 13), 
                "Ts2" = c(39, 51, 52, 40), 
                "Tl"  = c(31, 39, 40, 32), 
                "Tu2" = c(14, 21, 31, 32, 27), 
                "Tt"  = c(27, 32, 40, 52, 53)
            ) ),  
            stringsAsFactors = FALSE 
        ),  
        
        "scale" = NULL, 
        "over"  = NULL 
    ),  
    "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 63, 
        "SAND" = 2000 ) 
)   



# # {NAME} SOIL TEXTURE CLASSIFICATION
# # ----------------------------------------------------------

# soilTextEnvList[[ "hypres" ]] <- createTextureSystem( 
    # ternarySystem = createTernarySystem( 
        
        # "ternaryVariables" = createTernaryVariables( 
            # "blrNames"  = c( "SAND", "CLAY", "SILT" ),  # <<< CHECK
            # "blrLabels" = c(  
                # "Sand 0.05 - 2 mm [%]",                 # <<< CHECK
                # "Clay 0 - 0.002 mm [%]", 
                # "Silt 0.002 - 0.05 mm [%]" 
            # )   
        # ),  
        
        # "ternaryGeometry" = createTernaryGeometry( 
            # "tlrAngles" = c( 60, 60, 60 ),              # <<< CHECK
            # "blrClock"  = rep( TRUE, 3 ),               # <<< CHECK
            # "fracSum"   = 100 
        # ), 
        
        # "main" = "{name} soil texture classification",  # <<< CHANGE
        
        # "vertices" = data.frame( 
            # "id"    = c(),                              # <<< CHANGE
            # "CLAY"  = c(),                              # <<< CHANGE                         
            # "SILT"  = c(),                              # <<< CHANGE
            # "SAND"  = c()                               # <<< CHANGE
        # ),  
        
        # "classes" = data.frame( 
            # "abbrev" = c(),                             # <<< CHANGE
            # "name" = c(),                               # <<< CHANGE
            # "verticesId" = I( list() ),                 # <<< CHANGE
            # stringsAsFactors = FALSE 
        # ),  
        
        # "scale" = NULL, 
        # "over"  = NULL 
    # ),  
    # "particleSizeBoundaries" = c( 0, "CLAY" = 2, "SILT" = 50, 
        # "SAND" = 2000 )                                 # <<< CHECK
# )   



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

