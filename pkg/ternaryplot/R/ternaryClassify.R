
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+




#'Classify ternary data (determine the class to which ternary data points belong)
#'
#'Classify ternary data (determine the class to which ternary 
#'  data points belong)
#'
#'
#'@seealso \code{\link[ternaryplot]{ternaryClasses}}, to extract 
#'  the class polygons of a ternary classification from a 
#'  \code{\link[ternaryplot]{ternarySystem-class}} as a 
#'  \code{\link[ternaryplot]{ternaryPolygons-class}}.
#'
#'
#'@param s 
#'  Either \itemize{
#'    \item A \code{\link[ternaryplot]{ternarySystem-class}}. 
#'      That \code{ternarySystem-class} must contain 
#'      a ternary classification. 
#'    \item A single character string, the name of an existing 
#'      pre-defined \code{\link[ternaryplot]{ternarySystem-class}}.
#'    \item A \code{\link[ternaryplot]{ternaryPolygons-class}}.
#'  } 
#'
#'@param x 
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} containing point ternary data 
#'  to be classified. It should contain the 3 columns 
#'  names given by \code{blrNames(s)}. If missing, only the 
#'  ternary classification is drawn.
#'
#'@param method 
#'  A single character string, naming the classification method 
#'  to be used. Either \code{"over"}, in which case 
#'  \code{\link[sp]{over}} is used, or \code{"point.in.polygon"}, 
#'  in which case \code{\link[sp]{point.in.polygon}} is used.
#'  The method influence the format and content of the output 
#'  (see below).
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  When \code{method} is \code{"over"}, a vector of character 
#'  strings is returned, with the abbreviations of the 
#'  ternary classes in which each ternary points falls. One 
#'  value is returned per point, and when a point falls into 
#'  several classes (i.e. a tie: the point lies in between two or 
#'  more classes), \bold{the last class is returned}. This 
#'  means that the order in which the classes are defined 
#'  in \code{s} may influence the outcome of the classification.
#'  When \code{method} is \code{"point.in.polygon"}, a 
#'  \code{\link[base]{matrix}} is returned, where each row is 
#'  a data point and each column a ternary class, and each 
#'  cell gives the output of \code{\link[sp]{point.in.polygon}} 
#'  (\emph{"0: point is strictly exterior to pol; 1: point is 
#'  strictly interior to pol; 2: point lies on the relative 
#'  interior of an edge of pol; 3: point is a vertex of pol"}).
#'
#'
#'@rdname ternaryClassify-methods
#'
#'@example inst/examples/ternaryClassify-example.R
#'
#'@export 
#'
ternaryClassify <- function(
    s, 
    x, 
    ...
){  
    UseMethod( "ternaryClassify" ) 
}   


#'@rdname ternaryClassify-methods
#'
#'@method ternaryClassify character
#'
#'@export
#'
ternaryClassify.character <- function(
    s, 
    x, 
    method = "over", 
    ...
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    return( ternaryClassify.ternarySystem( s = s, x = x, 
        method = method, ... ) ) 
}   


#'@rdname ternaryClassify-methods
#'
#'@method ternaryClassify ternarySystem
#'
#'@export
#'
#'@importFrom sp over
#'@importFrom sp point.in.polygon
ternaryClassify.ternarySystem <- function(
    s, 
    x, 
    method = "over", 
    ...
){  
    if( method == "over" ){
        #   Convert ternary points to sp SpatialPoints
        spPts <- ternary2SpatialPoints.ternarySystem( s = s, x = x )
        
        #   Convert ternary classes to sp SpatialPolygonsDataFrame 
        spCls <- ternary2SpatialPolygons.ternarySystem( s = s )
        
        #   Classify the ternary data
        #   *   Fetch the class ID
        clsId <- sapply( spCls@polygons, function(x){x@ID} ) # slot(x,"ID")
        
        #   *   Classify
        out <- clsId[ sp::over( x = spPts, y = spCls ) ] 
        
        rm( spPts, spCls, clsId ) 
        
    }else if( method == "point.in.polygon" ){
        #   Extract the classes as polygons
        classes <- ternaryClasses.ternarySystem( s = s )
        
        #   ID column?
        idCol <- attr( x = classes, which = "idCol" ) 
        
        id <- classes[, idCol ]
        
        #   Convert the classes to x-y
        classes <- ternary2xy( s = s, x = classes )
        
        #   Convert the data points to x-y
        x <- ternary2xy( s = s, x = x )
        
        #   bottom right left column names:
        .blrNames <- blrNames.ternarySystem( s = s )
        
        #   Split the table of vertices by class:
        classes <- split(
            x = classes, 
            f = id
        )   
        
        #   Classify each point for each class
        out <- lapply(
            X   = classes, 
            FUN = function( cls ){
                return( point.in.polygon(
                    point.x      = x[, "x" ], 
                    point.y      = x[, "y" ], , 
                    pol.x        = cls[, "x" ], 
                    pol.y        = cls[, "y" ] ) )
            }   
        )   
        
        #   list -> matrix
        out <- do.call( what = "cbind", args = out )
        
        #   Set the column names
        colnames( out ) <- names( classes ) 
        
        rm( classes, idCol, .blrNames ) 
        
    }else{
        stop( sprintf( 
            "Unknown value for argument 'method': %s", 
            method 
        ) ) 
    }   
    
    return( out )
}   


#'@rdname ternaryClassify-methods
#'
#'@method ternaryClassify ternaryPolygons
#'
#'@export
#'
#'@importFrom sp over
#'@importFrom sp point.in.polygon
ternaryClassify.ternaryPolygons <- function(
    s, 
    x, 
    method = "over", 
    ...
){  
    #   Extract the ternarySystem
    terSys <- ternarySystem.ternaryPolygons( s = s )
    
    if( method == "over" ){
        #   Convert ternary points to sp SpatialPoints
        spPts <- ternary2SpatialPoints.ternarySystem( s = terSys, x = x )
        
        #   Convert ternary classes to sp SpatialPolygonsDataFrame 
        spCls <- ternary2SpatialPolygons.ternaryPolygons( s = s )
        
        #   Classify the ternary data
        #   *   Fetch the class ID
        clsId <- sapply( spCls@polygons, function(x){x@ID} ) # slot(x,"ID")
        
        #   *   Classify
        out <- clsId[ sp::over( x = spPts, y = spCls ) ] 
        
        rm( spPts, spCls, clsId ) 
        
    }else if( method == "point.in.polygon" ){
        #   Convert the classes to x-y
        classes <- ternary2xy( s = terSys, x = s )
        
        #   Convert the data points to x-y
        x <- ternary2xy( s = terSys, x = x )
        
        #   ID column?
        idCol <- attr( x = classes, which = "idCol" ) 
        
        #   bottom right left column names:
        .blrNames <- blrNames.ternarySystem( s = terSys )
        
        #   Split the table of vertices by class:
        classes <- split(
            x  = classes[, .blrNames ], 
            by = classes[, idCol ]
        )   
        
        #   Classify each point for each class
        out <- lapply(
            X   = classes, 
            FUN = function( cls ){
                return( point.in.polygon(
                    point.x      = x[, "x" ], 
                    point.y      = x[, "y" ], , 
                    pol.x        = cls[, "x" ], 
                    pol.y        = cls[, "y" ] ) )
            }   
        )   
        
        #   list -> matrix
        out <- do.call( what = "cbind", args = out )
        
        #   Set the column names
        colnames( out ) <- names( classes ) 
        
        rm( classes, idCol, .blrNames ) 
        
    }else{
        stop( sprintf( 
            "Unknown value for argument 'method': %s", 
            method 
        ) ) 
    }   
    
    return( out )
}   


