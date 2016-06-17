
library( "ternaryplot" ) 

hypres <- getTernarySystem( s = "hypres" )

ternaryClassNames( hypres ) 
# [1] "C"  "MF" "M"  "F"  "VF"

ternaryClassNames( hypres ) <- 
    tolower( ternaryClassNames( hypres ) )

ternaryClassNames( hypres )
# [1] "c"  "mf" "m"  "f"  "vf"

# # Plot the resulting ternary classification
# ternaryPlot( hypres ) 
