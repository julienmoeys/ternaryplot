
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                   |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



#'Converts x-y coordinates into ternary point-data.
#'
#'Converts x-y coordinates into ternary point-data (bottom, 
#'  left, right axis), according to the specification of a 
#'  \code{\link[ternaryplot]{ternarySystem-class}}
#'
#'
#'@param s
#'  A \code{\link[ternaryplot]{ternarySystem}} object or a 
#'  character string naming an pre-defined 
#'  \code{\link[ternaryplot]{ternarySystem-class}}.
#'
#'@param data
#'  A \code{\link[base]{data.frame}} or a 
#'  \code{\link[base]{matrix}} with x-y coordinates.
#'
#'@param xyNames
#'  A vector of 2 character strings. Names of the \code{x}- 
#'  and \code{y}-variables in table \code{data}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a \code{\link[base]{data.frame}} with ternary 
#'  point-data. As many rows as \code{nrow(data)} and 3 columns, 
#'  one per ternary-variable (same names as \code{blrNames(s)}). 
#'
#'
#'@rdname xy2ternary-methods
#'
#'@examples inst/examples/xy2ternary-example.R
#'
#'@export 
#'
xy2ternary <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    if( any( colInData <- !(xyNames %in% colnames( data )) ) ){
        stop( sprintf(
            "Column(s) %s missing in 'data' (see also argument 'xyNames')", 
            paste( xyNames[ colInData ], collapse = "; " ) 
        ) ) 
    }   
    
    if( missing( s ) ){ 
        UseMethod( "xy2ternary", object = character() )  
    }else{ 
        UseMethod( "xy2ternary" ) 
    }   
}   


#'@rdname xy2ternary-methods
#'
#'@method xy2ternary character
#'
#'@export
#'
xy2ternary.character <- function(
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    if( missing( s ) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s ) 
    }   
    
    return( xy2ternary.ternarySystem( s = s, data = data, 
        xyNames = xyNames, ... ) )
}   


#'@rdname xy2ternary-methods
#'
#'@method xy2ternary ternarySystem
#'
#'@export
#'
xy2ternary.ternarySystem <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    s <- .generateTernaryGeometry2ndClass( 
        s      = s, 
        class1 = "ternarySystem" 
    )   
    
    return( .xy2ternary( s = s, data = data, 
        xyNames = xyNames, ... ) ) 
}   


## #Internal geometry specific methods for xy2ternary
## #
## #Converts x-y coordinates into ternary point-data (bottom, 
## #  left, right axis), according to the specification of a 
## #  \code{\link[ternaryplot]{ternarySystem-class}}
## #
## #
## #@param s
## #  See xy2ternary
## #
## #@param data
## #  See xy2ternary
## #
## #@param \dots
## #  See xy2ternary
## #
## #
## #@return 
## #  See xy2ternary
## #
## #
## #@rdname xy2ternary-internal-methods
## #
## #@export 
## #
.xy2ternary <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    UseMethod( ".xy2ternary" ) 
}   


## #@rdname xy2ternary-internal-methods
## #
## #@method .xy2ternary geo_TTT
## #
## #@export
## #
.xy2ternary.geo_TTT <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    # #   If the triangle is undetermined, attribute to 
    # #   the bottom-left-right variables the names of the 
    # #   first 3 variables in x
    # s <- .fixTernarySystem( s = s, x = x ) 
    
    # Set some variables
    .blrClock  <- blrClock.ternarySystem( s ) 
    .tlrAngles <- tlrAngles.ternarySystem( s ) 
    .fracSum   <- fracSum.ternarySystem( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0 <- blrNames.ternarySystem( s = s ) 
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   Create an empty matrix that will contain the 
    #   converted values:
    tData <- as.data.frame( matrix(
        data = NA_real_, 
        ncol = length( blrNames0 ), 
        nrow = nrow( data ) 
    ) ) 
    colnames( tData ) <- blrNames0 
    
    
    #   The left side values are used to calculate y
    # y <- data[  , blrNames0[2] ] * sin( tlrAnglesRad[2] ) 
    tData[  , blrNames0[ 2L ] ] <- 
        data[, xyNames[ 2L ] ] / sin( tlrAnglesRad[ 2L ] ) 
    
    #   The bottom and right side values are used to 
    #   calculate x
    # x <- data[  , blrNames0[1] ] - y/tan( tlrAnglesRad[3] ) 
    tData[  , blrNames0[ 1L ] ] <- data[, xyNames[ 1L ] ] + 
        data[, xyNames[ 2L ] ] / tan( tlrAnglesRad[ 3L ] )
    
    #   "reverse" the bottom orientation to fit data 
    #   and y orientation:
    tData[  , blrNames0[ 1L ] ] <- ( .fracSum - tData[, blrNames0[ 1L ] ] ) 
    
    
    #   Set the last variable
    tData[  , blrNames0[ 3L ] ] <- 
        .fracSum - rowSums( tData[  , blrNames0[ c( 1L, 2L ) ] ] ) 
    
    
    return( tData )
}   



## #@rdname xy2ternary-internal-methods
## #
## #@method .xy2ternary geo_FFF
## #
## #@export
## #
.xy2ternary.geo_FFF <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    # #   If the triangle is undetermined, attribute to 
    # #   the bottom-left-right variables the names of the 
    # #   first 3 variables in x
    # s <- .fixTernarySystem( s = s, x = x ) 
    
    #   Set some variables
    .blrClock  <- blrClock.ternarySystem( s ) 
    .tlrAngles <- tlrAngles.ternarySystem( s ) 
    .fracSum   <- fracSum.ternarySystem( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames.ternarySystem( s = s )  
    
    
    #   Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   Create an empty matrix that will contain the 
    #   converted values:
    tData <- as.data.frame( matrix(
        data = NA_real_, 
        ncol = length( blrNames0 ), 
        nrow = nrow( data ) 
    ) ) 
    colnames( tData ) <- blrNames0 
     
    
    #   The right side value are used to calculate y
    # y <- data[  , blrNames0[3] ] * sin( tlrAnglesRad[3] )
    tData[  , blrNames0[ 3L ] ] <- 
        data[, xyNames[ 2L ] ] / sin( tlrAnglesRad[ 3L ] )
    
    #   The bottom and left side values are used to 
    #   calculate x
    # x <- data[  , blrNames0[1] ] + y/tan( tlrAnglesRad[2] ) 
    tData[, blrNames0[ 1L ] ] <- data[, xyNames[ 1L ] ] - 
        data[, xyNames[ 2L ] ]/tan( tlrAnglesRad[ 2L ] ) 
    
    
    #   Set the last variable
    tData[  , blrNames0[ 2L ] ] <- 
        .fracSum - rowSums( tData[  , blrNames0[ c( 1L, 3L ) ] ] ) 
    
    
    return( tData )
}   



## #@rdname xy2ternary-internal-methods
## #
## #@method .xy2ternary geo_FTX
## #
## #@export
## #
.xy2ternary.geo_FTX <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    # #   If the triangle is undetermined, attribute to 
    # #   the bottom-left-right variables the names of the 
    # #   first 3 variables in x
    # s <- .fixTernarySystem( s = s, x = x ) 
    
    # Set some variables
    .blrClock  <- blrClock.ternarySystem( s ) 
    .tlrAngles <- tlrAngles.ternarySystem( s ) 
    .fracSum   <- fracSum.ternarySystem( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames.ternarySystem( s = s )  
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   Create an empty matrix that will contain the 
    #   converted values:
    tData <- as.data.frame( matrix(
        data = NA_real_, 
        ncol = length( blrNames0 ), 
        nrow = nrow( data ) 
    ) ) 
    colnames( tData ) <- blrNames0 
    
    
    #   y coordinates calculated first based on the 
    #   left side
    # y <- data[  , blrNames0[2] ] * sin( tlrAnglesRad[2] )
    tData[  , blrNames0[ 2L ] ] <- 
        data[, xyNames[ 2L ] ] / sin( tlrAnglesRad[ 2L ] ) 
    
    #   x coordinates calculated with the bottom and left 
    #   side
    # x <- data[  , blrNames0[1] ] + y/tan( tlrAnglesRad[2] ) 
    tData[  , blrNames0[ 1L ] ] <- data[, xyNames[ 1L ] ] - 
        data[, xyNames[ 2L ] ] / tan( tlrAnglesRad[ 2L ] ) 
        
    
    #   In this case no axis need to be reverted
    
    
    #   Set the last variable
    tData[  , blrNames0[ 3L ] ] <- 
        .fracSum - rowSums( tData[  , blrNames0[ c( 1L, 2L ) ] ] ) 
    
    
    return( tData )
}   



## #@rdname xy2ternary-internal-methods
## #
## #@method .xy2ternary geo_TXF
## #
## #@export
## #
.xy2ternary.geo_TXF <- function( 
    s, 
    data, 
    xyNames = c( "x", "y" ), 
    ... 
){  
    # #   If the triangle is undetermined, attribute to 
    # #   the bottom-left-right variables the names of the 
    # #   first 3 variables in x
    # s <- .fixTernarySystem( s = s, x = x ) 
    
    # Set some variables
    .blrClock  <- blrClock.ternarySystem( s ) 
    .tlrAngles <- tlrAngles.ternarySystem( s ) 
    .fracSum   <- fracSum.ternarySystem( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames.ternarySystem( s = s )  
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   Create an empty matrix that will contain the 
    #   converted values:
    tData <- as.data.frame( matrix(
        data = NA_real_, 
        ncol = length( blrNames0 ), 
        nrow = nrow( data ) 
    ) ) 
    colnames( tData ) <- blrNames0 
    
    
    #   y coordinates are calculated from the right 
    #   side
    # y <- data[  , blrNames0[3] ] * sin( tlrAnglesRad[3] )
    tData[  , blrNames0[ 3L ] ] <- 
        data[, xyNames[ 2L ] ] / sin( tlrAnglesRad[ 3L ] )
    
    #   x coordinates calculated from the bottom 
    #   and right axis
    # x <- data[  , blrNames0[1] ] - y/tan( tlrAnglesRad[3] ) 
    tData[  , blrNames0[ 1L ] ] <- data[, xyNames[ 1L ] ] + 
        data[, xyNames[ 2L ] ] / tan( tlrAnglesRad[ 3L ] ) 
    
    
    #   Values on the bottom side are reverted
    tData[, blrNames0[ 1L ] ] <- ( .fracSum - tData[, blrNames0[ 1L ] ] ) 
    
    
    #   Set the last variable
    tData[  , blrNames0[ 2L ] ] <- 
        .fracSum - rowSums( tData[  , blrNames0[ c( 1L, 3L ) ] ] ) 
    
    
    return( tData )
}   

