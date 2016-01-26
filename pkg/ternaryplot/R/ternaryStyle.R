
# +--------------------------------------------------------+
# | Package:    ternaryplot                                |
# | Language:   R + roxygen2 inline documentation          |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>        |
# | License:    AGPL3, Affero General Public License       |
# |             version 3                                  |
# +--------------------------------------------------------+



#' Style for ternary plots (predefined sets of graphical parameters)
#'
#' Style for ternary plots (predefined sets of graphical 
#'  parameters)
#'
#'
#'@seealso \code{\link[ternaryplot]{ternaryPlot}}, that can 
#'  be called \emph{after} \code{ternaryStyle} has been 
#'  called (and which uses these graphical parameters). 
#'
#'
#'@param style 
#'  Single character string. Name of the style to be used. 
#'  Possible values are \code{publication} and \code{ggplot2}.
#'
#'@param margin
#'  Single logical value. If \code{TRUE}, the function also 
#'  sets narrower plot margins, so that the ternary plot 
#'  can be (for example) a bit larger. Note that not all 
#'  style may set the margins.
#'
#'
#'@example inst/examples/ternaryStyle-example.R
#'
#'@rdname ternaryStyle-methods
#'
#'@export
#'
ternaryStyle <- function( 
    style, 
    margin = FALSE 
){  
    if( missing( style ) ){ 
        UseMethod( "ternaryStyle", object = character(0) ) 
    }else{ 
        UseMethod( "ternaryStyle" ) 
    }   
}   


#'@rdname ternaryStyle-methods
#'
#'@method ternaryStyle character
#'
#'@export
#'
ternaryStyle.character <- function( 
    style, 
    margin = FALSE 
){  
    if( missing( style ) ){ 
        style <- "publication"
    }   
    
    if( style == "publication" ){
        #   Set general graphical parameters (enlarged plot)
        par( 
            family    = "serif", 
            font      = 2, 
            font.axis = 2, 
            font.lab  = 2,
            lwd       = 2 ) 
        
        #   Set the margins
        if( margin ){
            # mar = c(bottom, left, top, right) 
            par( mar = c(3,2,0,2)+.1 ) 
        }   
        
        #   Set specific graphical parameters
        tpPar( 
            grid.line.col    = "white", 
            # arrowsBreak      = FALSE, 
            plot.bg          = gray( .95 ), 
            axis.line.lwd    = 2, 
            class.border.lwd = 2, 
            ticks.line.lwd   = 2, 
            grid.line.lwd    = 2 )
        
    }else if( style == "ggplot2" ){
        #   Set general graphical parameters (enlarged plot)
        par( 
            family    = "sans", 
            font      = 2, 
            font.axis = 2, 
            font.lab  = 2, 
            col.lab   = "darkgray", 
            lwd       = 2 ) 
        
        #   Set the margins
        if( margin ){
            # mar = c(bottom, left, top, right) 
            par( mar = c(3,2,0,2)+.1 ) 
        }   
        
        #   Set specific graphical parameters
        tpPar( 
            grid.line.col    = "white", 
            arrows           = FALSE, 
            # arrowsBreak      = FALSE, 
            plot.bg          = gray( .95 ), 
            axis.line.lwd    = 2, 
            axis.line.col    = NA, 
            class.border.lwd = 2, 
            ticks.line.lwd   = 2, 
            ticks.line.col   = "darkgray", 
            grid.line.lwd    = 2 )
        
    }else{
        stop( sprintf(
            "Unknown/unsupported value for 'style' argument: %s", 
            style 
        ) ) 
    }   
    
}   