
rm(list=ls(all=TRUE)) 
pkgName     <- "ternaryplot"
pkgDir      <- "C:/Users/julienm/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot/pkg"


library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "0.6.4", 
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
