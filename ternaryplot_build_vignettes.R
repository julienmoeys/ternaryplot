
rm(list=ls(all=TRUE)) 

pkgName <- "ternaryplot"

if( tolower( Sys.info()[[ "sysname" ]] ) == "linux" ){
    pkgDir <- sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.info()[[ "user" ]], pkgName )
}else{
    pkgDir <- sprintf( 
        "%s/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.getenv("dropboxPath"), pkgName )
}   

#   Stylesheet for the generated HTML files
stylesheet <- file.path( pkgDir, pkgName, "vignettes/markdown.ternaryplot.css" )

wd <- getwd()


# LOAD REQUIRED LIBRARIES
# ==========================================================
library( "markdown" ) 
library( "knitr" ) 



# MAIN CODE
# ==========================================================

fList <- c(
    "vignettes/ternaryplot-graphical-parameters.Rmd" = "doc/ternaryplot-graphical-parameters.html" 
)   

setwd( tempdir() )

for( i in 1:length( fList ) ){
    knitr::knit2html(
        input      = file.path( pkgDir, pkgName, names( fList )[ i ] ), 
        output     = file.path( pkgDir, pkgName, "inst", as.character( fList[ i ] ) ), 
        envir      = e <- new.env(), 
        encoding   = "UTF-8", 
        stylesheet = stylesheet, 
        quiet      = TRUE ) 
}

setwd( wd )
