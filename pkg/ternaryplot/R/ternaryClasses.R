
#'Extract the ternary classification (class polygons) from a ternarySystem-object
#'
#'Extract the ternary classification (class polygons) from a 
#'  ternarySystem-object.
#'
#'
#'@seealso \code{\link[ternaryplot]{ternary2SpatialPolygonsDataFrame}} 
#'  (to convert the grid into a 
#'  \code{\link[sp]{SpatialPolygonsDataFrame}}), and 
#'  \code{\link[ternaryplot]{ternaryPlot}} to plot the 
#'  output. 
#'
#'
#'@param s 
#'  A \code{\link[ternaryplot]{ternarySystem}}-object (such 
#'  as created with \code{\link[ternaryplot]{createTernarySystem}} 
#'  or fetched with \code{\link[ternaryplot]{getTernarySystem}}), 
#'  or a single character string naming a 
#'  \code{\link[ternaryplot]{ternarySystem}}.
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A \code{ternaryPolygons}-object 
#'  containing the coordinates of the class polygons 
#'  extracted from \code{s}. 
#'
#'
#'@rdname ternaryClasses-methods
#'
#'@example inst/examples/ternaryClasses-example.R
#'
#'@export 
#'
ternaryClasses <- function(
    s, 
    ...
){  
    if( missing(s) ){ 
        UseMethod( "ternaryClasses", object = character(0) ) 
    }else{ 
        UseMethod( "ternaryClasses" ) 
    }   
    
    UseMethod( "ternaryClasses" ) 
}   


#'@rdname ternaryClasses-methods
#'
#'@method ternaryClasses character
#'
#'@export
#'
ternaryClasses.character <- function(
    s, 
    ...
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    ternaryClasses( s = s, ... ) 
}   


#'@rdname ternaryClasses-methods
#'
#'@method ternaryClasses ternarySystem
#'
#'@export
#'
ternaryClasses.ternarySystem <- function(
    s, 
    ...
){  
    classes <- s[[ "classes" ]] 
    
    if( nrow( classes ) > 0 ){
        grd <- lapply(
            X   = 1:nrow( classes ), 
            FUN = function(i){
                verticesId <- unlist( classes[ i, "verticesId" ] )
                
                vertices <- s[[ "vertices" ]]
                rownames( vertices ) <- 
                    as.character( s[[ "vertices" ]][, "id" ] ) # Just in case?
                
                vertices <- vertices[ 
                    as.character( verticesId ), 
                    colnames( vertices ) != "id" 
                ]   
                
                vertices[, "abbrev" ] <- classes[ i, "abbrev" ] 
                
                return( vertices )
            }   
        )   
        
        #   list -> data.frame
        grd <- do.call( what = "rbind", args = grd ) 
        
        #   Rename column "abbrev" to "id" (polygon id)
        colnames( grd )[ colnames( grd ) == "abbrev" ] <- "id" 
    }else{
        grd <- s[[ "vertices" ]][ logical(0), ] # Make sure no row is selected
    }   
    
    grd <- list( 
        "grid"          = grd, 
        "ternarySystem" = s 
    )   
    
    class( grd ) <- "ternaryPolygons" 
    
    return( grd )
}   

