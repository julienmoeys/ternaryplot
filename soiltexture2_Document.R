
rm(list=ls(all=TRUE)) 

pkgName <- "soiltexture2"
project <- "ternaryplot"

if( tolower( Sys.info()[[ "sysname" ]] ) == "linux" ){
    pkgDir <- sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.info()[[ "user" ]], project )
}else{
    pkgDir <- sprintf( 
        "%s/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.getenv(x = "dropboxPath" ), project )
}   

library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "0.2.0", 
    pkgDepends  = "ternaryplot", # "MASS"
    pkgSuggests = "knitr", 
    pkgImports  = NULL, 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 

