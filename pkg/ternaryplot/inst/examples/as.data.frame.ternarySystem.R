
library( "ternaryplot" )

hypres <- getTernarySystem( "hypres" )

vertices <- as.data.frame( hypres, what = 'vertices' ) 
vertices 

centroids <- as.data.frame( hypres, what = 'centroids' ) 
centroids

    # # Visualise the result:

    # ternaryStyle( "publication" )
    # ternaryPlot( hypres )
    # ternaryPoints( s = hypres, x = vertices, col = "darkred" ) 
    # ternaryPoints( s = hypres, x = centroids, col = "purple" )  
