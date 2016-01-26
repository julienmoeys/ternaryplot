
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                     |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



# createTernaryGrid =============================================

#'Create a regular ternary grid. Base frame for binning ternary data.
#'
#'Create a regular ternary grid. Base frame for binning ternary data.
#'
#'
#'@seealso \code{\link[ternaryplot]{ternary2SpatialPolygons}} 
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
#'@param n
#'  Single integer value. Number of grid cells (not grid 
#'  nodes)
#'
#'@param \dots 
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  A ternary grid (of polygons), with class \code{ternaryPolygons}.
#'
#'
#'@example inst/examples/createTernaryGrid-example.R
#'
#'
#'@rdname createTernaryGrid-methods
#'
#'@export 
#'
createTernaryGrid <- function(
 s, 
 ... 
){  
    if( missing( s ) ){ 
        UseMethod( "createTernaryGrid", object = character(0) ) 
    }else{ 
        UseMethod( "createTernaryGrid" ) 
    }   
}   



#'@rdname createTernaryGrid-methods
#'
#'@method createTernaryGrid character
#'
#'@export
#'
createTernaryGrid.character <- function(
 s, # A ternarySystem
 ... 
){  
    if( missing(s) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s ) 
    }   
    
    createTernaryGrid( s = s, ... )
}   



#'@rdname createTernaryGrid-methods
#'
#'@method createTernaryGrid ternarySystem
#'
#'@export
#'
createTernaryGrid.ternarySystem <- function(
 s,         # A ternarySystem
 n = 10,    # number of intervals
 ... 
){  
    x <- y <- seq( from = 0, to = 1, length.out = n + 1L ) 
    
    empty <- data.frame( "id" = character(0), "x" = numeric(0), 
        "y" = numeric(0), "z" = numeric(0), stringsAsFactors = FALSE )
    
    grd <- lapply( 
        X   = 1:(length(x)-1), 
        FUN = function(i){ 
            grd <- lapply( 
                X   = 1:(length(x)-1), 
                FUN = function(j){ 
                    # i <- 3; j <- 2
                    
                    df1 <- cbind( 
                        "x" = x[ c( i, i+1, i,   i, i ) ], 
                        "y" = x[ c( j, j,   j+1, j, j ) ] 
                    )   
                    
                    rs1 <- rowSums( df1 )
                    
                    if( any( rs1 > 1 ) ){ # any( !is.na( over( df1, triangleFrame ) ) )
                        df1 <- empty
                    }else{ 
                        df1 <- data.frame( "id" = paste( i, j, 1, sep = ":" ), 
                            df1, "z" = 1 - rs1, stringsAsFactors = FALSE ) 
                    };  rm( rs1 ) 
                    
                    
                    df2 <- cbind( 
                        "x" = x[ c( i+1, i+1, i,   i+1, i+1 ) ], 
                        "y" = x[ c( j,   j+1, j+1, j,   j   ) ] 
                    )   
                    
                    rs2 <- rowSums( df2 )
                    
                    if( any( rs2 > 1 ) ){ # any( !is.na( over( df2, triangleFrame ) ) )
                        df2 <- empty
                    }else{ 
                        df2 <- data.frame( "id" = paste( i, j, 2, sep = ":" ), 
                            df2, "z" = 1 - rs2, stringsAsFactors = FALSE ) 
                    };  rm( rs2 ) 
                    
                    
                    
                    return( rbind( df1, df2 ) ) 
                }   
            )   
            
            grd <- do.call( "what" = "rbind", args = grd )
        }   
    )   
    
    grd <- do.call( "what" = "rbind", args = grd )
    
    .blrNames <- blrNames( s = s )
    .fracSum <- fracSum( s = s ) 
    
    colnames( grd ) <- c( "id", .blrNames ) 
    
    #   Convert to the right frac sum (1 [-] or 100 [%])
    grd[, .blrNames ] <- grd[, .blrNames ] * .fracSum
    
    grd <- list( 
        "grid"          = grd, 
        "ternarySystem" = s, 
        "labels"        = NULL 
    )   
    
    class( grd ) <- "ternaryPolygons" 
    
    return( grd )
}   


