
# +-------------------------------------------------------------+
# | Package:    ternaryplot                                   |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+



#'Converts angles expressed in degrees into radian.
#'
#'Converts angles expressed in degrees into radian.
#'
#'@param angle
#'  Single numeric. Angle in Degrees.
#'
#'@export
#'
deg2rad <- function(
    angle
){  
    (pi/180)*angle
}   




#'Converts ternary point-data into x-y coordinates
#'
#'Converts ternary point-data (bottom, left, right axis) into 
#'  x-y coordinates, according to the specification of a 
#'  \code{ternarySystem}
#'
#'
#'@param s
#'  A \code{\link[ternaryplot]{ternarySystem}} object or a 
#'  character string naming an pre-defined \code{ternarySystem}, 
#'  or a \code{\link[ternaryplot]{ternaryData}} object. For the 
#'  latter, if \code{x} is missing, \code{s} will also be used 
#'  for \code{x}
#'
#'@param x
#'  A \code{\link[base]{data.frame}} or a \code{\link[base]{matrix}} 
#'  with ternary point-data. Can be missing if \code{s} is a 
#'  \code{\link[ternaryplot]{ternaryData}} object.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a \code{\link[base]{data.frame}} with point-data in 
#'  (columns) x and y. 
#'
#'
#'@rdname ternary2xy-methods
#'
#'@export 
#'
ternary2xy <- function( 
 s, 
 ... 
){  
    if( missing( s ) ){ 
        UseMethod( "ternary2xy", object = character() )  
    }else{ 
        UseMethod( "ternary2xy" ) 
    }   
}   



#'@rdname ternary2xy-methods
#'
#'@method ternary2xy character
#'
#'@export
#'
ternary2xy.character <- function(
 s, 
 ... 
){  
    if( missing( s ) ){ 
        s <- getTernarySystem() 
    }else{ 
        s <- getTernarySystem( s = s ) 
    }   
    
    ternary2xy.ternarySystem( s = s, ... )
}   



#'@rdname ternary2xy-methods
#'
#'@method ternary2xy ternaryData
#'
#'@export
#'
ternary2xy.ternaryData <- function(
 s, 
 ... 
){  
    if( missing(x) ){ 
        x <- s 
    }   
    
    s <- attr( x = s, which = "ternarySystem" ) 
    
    ternary2xy( s = s, x = x, ... )
}   


#'@rdname ternary2xy-methods
#'
#'@method ternary2xy ternarySystem
#'
#'@export
#'
ternary2xy.ternarySystem <- function( 
 s, 
 x, 
 ... 
){  
    # Test if the ternary data are conform 
    if( !"ternaryData" %in% class( x ) ){ 
        x <- ternaryData( s = s, x = x ) 
    }   
    
    
    class( s ) <- .generateTernaryGeometry2ndClass( 
        blrClock = blrClock( s ), 
        class1   = "ternarySystem" 
    )   
    
    
    return( .ternary2xy( s = s, x = x, ... ) ) 
}   



    # # library( "ternaryplot" ) 
    
    # f1 <- runif( 10, 0, 1 ) 
    # f2 <- runif( 10, 0, 1 ) 
    # f3 <- runif( 10, 0, 1 ) 
    
    # f1b <- f1 / (f1 + f2 +f3) 
    # f2b <- f2 / (f1 + f2 +f3) 
    # f3b <- f3 / (f1 + f2 +f3) 
    # rm( f1, f2, f3 ) 
    
    # tbl <- data.frame( "F1" = f1b, "F2" = f2b, "F3" = f3b ) * 100  # 100%
    # rm( f1b, f2b, f3b ) 
    # # ternaryData( tbl )
    
    # tbl <- ternary2xy( s = "default", x = tbl ) 
    
    # fr <- data.frame( 
        # "F1" = c( 100, 000, 000 ), 
        # "F2" = c( 000, 100, 000 ), 
        # "F3" = c( 000, 000, 100 ) )
    
    # fr <- ternary2xy( "default", x = fr,  ) 
    
    # plot( x = 50, y = 50, xlim = c(0,100), ylim = c(0,100), 
        # type = "n", bty = "n" )  
    
    # polygon( x = fr$"x", y = fr$"y", border = "red" )  
    
    # points( x = tbl$"x", y = tbl$"y" ) 





## # Internal geometry specific methods for ternary2xy
## #
## #Converts ternary point-data (bottom, left, right axis) into 
## #  x-y coordinates, according to the specification of a 
## #  \code{ternarySystem}
## #
## #
## #@param s
## #  See ternary2xy
## #
## #@param x
## #  See ternary2xy
## #
## #@param \dots
## #  See ternary2xy
## #
## #
## #@return 
## #  See ternary2xy
## #
## #
## #@rdname ternary2xy-internal-methods
## #
## #@export 
## #
.ternary2xy <- function( 
 s, 
 ... 
){  
    UseMethod( ".ternary2xy" ) 
}   



## #@rdname ternary2xy-internal-methods
## #
## #@method .ternary2xy geo_TTT
## #
## #@export
## #
.ternary2xy.geo_TTT <- function( 
 s, 
 x, 
 ... 
){  
    # Set some variables
    .blrClock  <- blrClock( s ) 
    .tlrAngles <- tlrAngles( s ) 
    .fracSum   <- fracSum( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames( s = s )  
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   "reverse" the bottom and right orientation to fit x 
    #   and y orientation:
    x[  , blrNames0[ 1L ] ] <- ( .fracSum - x[, blrNames0[ 1L ] ] ) 
    x[  , blrNames0[ 3L ] ] <- ( .fracSum - x[, blrNames0[ 3L ] ] ) 
    
    
    #   The left side values are used to calculate y
    y <- x[  , blrNames0[2] ] * sin( tlrAnglesRad[2] ) 
    
    #   The bottom and right side values are used to 
    #   calculate x
    x <- x[  , blrNames0[1] ] - y/tan( tlrAnglesRad[3] ) 
    
    return( data.frame( "x" = x , "y" = y ) )
}   



## #@rdname ternary2xy-internal-methods
## #
## #@method .ternary2xy geo_FFF
## #
## #@export
## #
.ternary2xy.geo_FFF <- function( 
 s, 
 x, 
 ... 
){  
    #   Set some variables
    .blrClock  <- blrClock( s ) 
    .tlrAngles <- tlrAngles( s ) 
    .fracSum   <- fracSum( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames( s = s )  
    
    
    #   Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   "reverse" the left side to fit x and y orientation:
    x[ , blrNames0[ 2L ] ] <- ( .fracSum - x[, blrNames0[ 2L ] ] ) 
     
    
    #   The right side value are used to calculate y
    y <- x[  , blrNames0[3] ] * sin( tlrAnglesRad[3] )
    
    #   The bottom and left side values are used to 
    #   calculate x
    x <- x[  , blrNames0[1] ] + y/tan( tlrAnglesRad[2] ) 
    
    return( data.frame( "x" = x , "y" = y ) )
}   



## #@rdname ternary2xy-internal-methods
## #
## #@method .ternary2xy geo_FTX
## #
## #@export
## #
.ternary2xy.geo_FTX <- function( 
 s, 
 x, 
 ... 
){  
    # Set some variables
    .blrClock  <- blrClock( s ) 
    .tlrAngles <- tlrAngles( s ) 
    .fracSum   <- fracSum( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames( s = s )  
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   In this case no axis need to be reverted
     
    
    #   y coordinates calculated first based on the 
    #   left side
    y <- x[  , blrNames0[2] ] * sin( tlrAnglesRad[2] )
    
    #   x coordinates calculated with the bottom and left 
    #   side
    x <- x[  , blrNames0[1] ] + y/tan( tlrAnglesRad[2] ) 
    
    return( data.frame( "x" = x , "y" = y ) )
}   



## #@rdname ternary2xy-internal-methods
## #
## #@method .ternary2xy geo_TXF
## #
## #@export
## #
.ternary2xy.geo_TXF <- function( 
 s, 
 x, 
 ... 
){  
    # Set some variables
    .blrClock  <- blrClock( s ) 
    .tlrAngles <- tlrAngles( s ) 
    .fracSum   <- fracSum( s ) 
    # fracSumTol <- getTpPar( par = "fracSumTol" ) * fracSum 
    
    blrNames0   <- blrNames( s = s )  
    
    
    # Angle transformation: degree to radian
    tlrAnglesRad <- deg2rad( angle = .tlrAngles )
    
    
    #   Values on the botton side are reverted
    x[, blrNames0[ 1L ] ] <- ( .fracSum - x[, blrNames0[ 1L ] ] ) 
     
    
    #   y coordinates are calculated from the right 
    #   side
    y <- x[  , blrNames0[3] ] * sin( tlrAnglesRad[3] )
    
    #   x coordinates calculated from the bottom 
    #   and right axis
    x <- x[  , blrNames0[1] ] - y/tan( tlrAnglesRad[3] ) 
    
    return( data.frame( "x" = x , "y" = y ) )
}   

