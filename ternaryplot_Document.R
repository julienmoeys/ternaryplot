
rm(list=ls(all=TRUE)) 
pkgName     <- "ternaryplot"
if( tolower( Sys.info()[[ "sysname" ]] ) == "linux" ){
    pkgDir <- sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot", 
        Sys.info()[[ "user" ]] )
}else{
    pkgDir <- sprintf( 
        "C:/Users/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot/pkg", 
        Sys.info()[[ "user" ]] )
}   

library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "0.6.7", 
    pkgDepends  = NULL, # "MASS"
    pkgSuggests = NULL, 
    pkgImports  = "sp", 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 
