
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+



#'Count the number of ternary data point falling in each cell or class 
#'
#'Count the number of ternary data point falling in each cell 
#'  of a systematic triangular grid (if \code{grid=TRUE}) or 
#'  falling in each class of a ternary classification 
#'  (if \code{grid=FALSE}) or in each polygon of a 
#'  \code{\link[ternaryplot]{ternaryPolygons-class}} object 
#'  (if \code{s} is a 
#'  \code{\link[ternaryplot]{ternaryPolygons-class}} object). 
#'  This function is used as a basis for the plot function 
#'  \code{\link[ternaryplot]{ternaryBins}} (binning and plotting 
#'  bins of ternary data)
#'
#'
#'@seealso \code{\link[ternaryplot]{createTernaryGrid}} for 
#'  generating a systematic triangular grid and 
#'  \code{\link[ternaryplot]{ternaryBins}} for binning and 
#'  plotting bins of ternary data
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
#'  to be binned and counted. It should contain the 3 columns 
#'  names given by \code{blrNames(s)}.
#'
#'@param grid 
#'  Single logical value. Set to \code{TRUE} (the default) 
#'  to retrieve counts for a systematic ternary grid, and 
#'  to \code{FALSE} to count the number of \code{x}-data 
#'  points per class in \code{s} instead.
#'
#'@param \dots 
#'  Additional parameters passed to specific methods. Not used.
#'
#'
#'@return 
#'  Returns a \code{\link[ternaryplot]{ternaryPolygons-class}} 
#'  object with one extra \code{\link[base]{attr}}ibute, 
#'  \code{counts}. \code{counts} is a tagged vector with as 
#'  many values as grid-cells (if \code{grid} is \code{TRUE}) 
#'  or as many values as classes in \code{s} (if 
#'  \code{grid} is \code{FALSE}). The tags are the grid-cells 
#'  identifier or the classes abbreviations. 
#'
#'
#'@rdname ternaryCount-methods
#'
#'@example inst/examples/ternaryCount-example.R
#'
#'@export 
#'
ternaryCount <- function(
    s, 
    x, 
    ...
){  
    UseMethod( "ternaryCount" ) 
}   


#'@rdname ternaryCount-methods
#'
#'@method ternaryCount character
#'
#'@export
#'
ternaryCount.character <- function(
    s, 
    x, 
    grid = TRUE, 
    ...
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s )  
    }   
    
    return( ternaryCount.ternarySystem( s = s, x = x, 
        grid = grid, ... ) ) 
}   


#'@rdname ternaryCount-methods
#'
#'@method ternaryCount ternarySystem
#'
#'@export
#'
ternaryCount.ternarySystem <- function(
    s, 
    x, 
    grid = TRUE, 
    ...
){  
    #   Generate a systematic triangular grid
    if( grid ){
        grd <- createTernaryGrid.ternarySystem( s = s ) 
    }else{
        grd <- ternaryClasses.ternarySystem( s = s )
    }   
    
    return( ternaryCount.ternaryPolygons( s = grd, x = x ) )
}   


#'@rdname ternaryCount-methods
#'
#'@method ternaryCount ternaryPolygons
#'
#'@export
#'
ternaryCount.ternaryPolygons <- function(
    s, 
    x, 
    ...
){  
    #   Classify the dataset
    grdClasses <- ternaryClassify( s = s, x = x, 
        method = "over" ) 
    
    #   Fetch the name of the identifier column
    idCol <- attr( x = s, which = "idCol" )
    
    #   Count the number of points per grid cell
    counts <- table( grdClasses, dnn = "Values" ) 
    #   table-object -> data.frame
    counts <- as.data.frame( counts, stringsAsFactors = FALSE ) 
    #   data.frame -> vector of integer with tags
    counts <- structure( counts[, "Freq" ], .Names = counts[, "Values" ] )
    
    #   Fetch all the grid cells in the right order
    counts <- counts[ unique( s[, idCol ] ) ]
    #   Tag all the values
    names( counts ) <- unique( s[, idCol ] ) 
    #   Set NA-values to 0
    counts[ is.na( counts ) ] <- 0 
    
    #   Set the counts as attributes of the 
    #   ternaryPolygons-class grid
    attr( x = s, which = "counts" ) <- counts
    
    return( s )
}   

