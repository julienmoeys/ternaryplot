
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License v 3   |
# +--------------------------------------------------------+



# .setTernarySystem =============================================

    # # Set and check the ternarySystem. Used inside functions.
    # .setTernarySystem <- function( s = "default" ){ 
        # if( is.character( s ) ){ 
            # s <- getTernarySystem( s = s ) 
        # }else if( !is( s, "ternarySystem" ) ){ 
            # stop( "'s' must be a 'character' or a 'ternarySystem'" ) 
        # }
        
        # # validObject( s ) # Create an infinite loop!
        
        # return( s )
    # }   


# ternaryData ==============================================

    ## #Converts tabular data to ternary data, and perform some data-checks
    ## #
    ## #Converts tabular data to ternary data, and perform some data-checks
    ## #
    ## #
    ## #@param s 
    ## #  A \code{ternarySystem} object, as created with 
    ## #  \code{\link[ternaryplot]{createTernarySystem}}, or a single 
    ## #  \code{character} string. Can be missing.
    ## #
    ## #@param x 
    ## #  A \code{\link[base]{data.frame}} or a \code{\link[base]{matrix}} 
    ## #  containing point ternary data (x-y-x) to be tested.
    ## #
    ## #@param .blrNames 
    ## #  See \code{\link[ternaryplot]{blrNames}}
    ## #
    ## #@param .fracSum 
    ## #  See \code{\link[ternaryplot]{fracSum}}
    ## #
    ## #@param testRange 
    ## #  Single logical. Test if the range of fraction is between 0 and 
    ## #  the expected sum of fractions (1 or 100). 
    ## #
    ## #@param testSum 
    ## #  Single logical. Test if the sum of the 3 fractions is equal to 
    ## #  the expected sum of fractions (1 or 100).
    ## #
    ## #@param \dots
    ## #  Additional parameters passed to \code{ternarySystem}-methods.
    ## #
    ## #@return 
    ## #  Returns a \code{ternaryData} \code{\link[base]{data.frame}} 
    ## #  object.
    ## #
    ## #
    ## #@rdname ternaryData-methods
    ## #
    ## #@export 
    ## #
    # ternaryData <- function( 
        # s, 
        # ... 
    # ){    
        # UseMethod( "ternaryData" ) 
    # }   


    ## #@rdname ternaryData-methods
    ## #
    ## #@method ternaryData character
    ## #
    ## #@export
    ## #
    # ternaryData.character <- function( 
     # s, 
     # ...
    # ){  
       # s <- getTernarySystem( s = s ) 
       
       # ternaryData.ternarySystem( s = s, ... )
    # }   



## #@rdname ternaryCheck-methods
## #
## #@method ternaryCheck data.frame
## #
## #@export
## #
.ternaryCheck.data.frame <- function( 
 s, 
 x, 
 testRange, 
 testSum, 
 ... 
){  
    .tpPar <- tpPar()
    
    # geo     <- ternaryGeometry( s )  
    # var   <- ternaryVariables( s ) 
    
    
    if( is.matrix( x ) ){ 
        x <- as.data.frame( x ) 
        
    }else if( (!is.data.frame( x )) ){ # & (!"data.table" %in% class(x))
        stop( "'x' should be a data.frame or a matrix" )
    }   
    
    
    #   If the triangle is undetermined, attribute to 
    #   the bottom-left-right variables the names of the 
    #   first 3 variables in x
    s <- .fixTernarySystem( s = s, x = x ) 
    
    
    cn <- colnames( x ) 
    
    # if( is.null( .blrNames ) ){ 
    
    .blrNames   <- blrNames.ternarySystem( s ) 
    
    testCol <- .blrNames %in% cn
    
    if( any( !testCol ) ){ 
        stop( sprintf( 
            "Some column missing in 'x' (%s)", 
            paste( .blrNames[ !testCol ], collapse = "; " ) 
        ) ) 
    };  rm( testCol ) 
    
    .fracSum <- fracSum.ternarySystem( s ) 
    
    
    # Tolerance:
    fracSumTol <- .tpPar[[ "fracSumTol" ]] * .fracSum 
    
    
    # Check missing values
    if( any( is.na( x[, .blrNames ] ) ) ){ 
        stop( "Some values in 'x' are missing. Missing values are not allowed" )   
    }   
    
    
    if( testRange & (nrow( x ) != 0) ){ 
        # Check that no fraction is negative
        if( any( x[, .blrNames ] < 0 ) ){ 
            stop( "Some fractions in 'x' are negative. Fractions can't be negative" ) 
        }   
        
        # Check that no fraction is over .fracSum 
        if( any( x[, .blrNames ] > .fracSum ) ){ 
            stop( sprintf( 
                "Some value(s) in 'x' are bigger than the expected sum of fractions (%s).\n  Fix your data, use ternaryNormalise().", 
                .fracSum 
            ) ) 
        }   
    }   
    
    if( testSum & (nrow( x ) != 0) ){ 
        # Check the fractions' sum
        testFracSum <- rowSums( x[, .blrNames ] ) 
        
        # Within accepted bounds?
        testFracSum <- 
            (testFracSum >= (.fracSum - fracSumTol)) & 
            (testFracSum <= (.fracSum + fracSumTol))   
        
        if( any( !testFracSum ) ){ 
            stop( sprintf( 
                "The sum of fractions of some rows in 'x' differ from the expected sum of fractions (%s).\n  Fix your data, use ternaryNormalise(), or increase 'fracSumTol' (see ?rmPar).", 
                .fracSum 
            ) ) 
        }   
    }   
    
    return( x ) 
}   

    # tbl <- data.frame(
        # "F1" = c(0.07, 0.27, 0.07, 0.27, 0.07, 0.4, 0.6, 0.4, 0.73 ), 
        # "F2" = c(0.07, 0.13, 0.4, 0.46, 0.73, 0.07, 0.13, 0.4, 0.07 ), 
        # "F3" = c(0.86, 0.6, 0.53, 0.27, 0.2, 0.53, 0.27, 0.2, 0.2 ) )  
    
    # (tbl2 <- ternaryCheck( x = tbl * 100, s = "default",  ))
    # attr( tbl2, "ternarySystem" ) 
    
    # (tbl2 <- ternaryCheck( x = tbl, s = "default", .fracSum = 1 )) 
    
    # cn <- c( "A1", "A2", "A3" ) 
    # colnames( tbl ) <- cn 
    
    # (tbl2 <- ternaryCheck( x = tbl, s = "default", .fracSum = 1, 
        # .blrNames = cn ))
    
    # tbl <- data.frame( tbl * 100, "id" = 1:nrow(tbl) )
    
    # (tbl2 <- ternaryCheck( x = tbl, s = "default", 
        # .blrNames = cn ) ) 

