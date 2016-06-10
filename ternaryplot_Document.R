
rm(list=ls(all=TRUE)) 

pkgName <- "ternaryplot"

if( tolower( Sys.info()[[ "sysname" ]] ) == "linux" ){
    pkgDir <- sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.info()[[ "user" ]], pkgName )
}else{
    pkgDir <- sprintf( 
        "%s/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.getenv(x = "dropboxPath" ), pkgName )
}   

library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "0.11.0", 
    pkgDepends  = NULL, # "MASS"
    pkgImports  = c( "sp", "graphics", "grDevices", "utils" ), 
    pkgSuggests = "knitr", 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 

