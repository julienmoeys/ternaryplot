
rm(list=ls(all=TRUE)) 

pkgName <- "ternaryplot"

if( Sys.info()[["sysname"]] == "Linux" ){
    setwd( sprintf( 
        "/home/%s/Dropbox/_WORK/_PROJECTS/r_packages/ternaryplot/pkg/ternaryplot", 
        Sys.info()[[ "user" ]]
    ) )  
}else{
    pkgDir <- sprintf( 
        "%s/_WORK/_PROJECTS/r_packages/%s/pkg", 
        Sys.getenv("dropboxPath"), pkgName )
}   

#   Files to be sourced first (order matters)
sourceFiles <- c(
    "aa00-ternaryplot-package.R", 
    "aa01-ternaryplot-options.R", 
    "aa02-ternaryplot-classes.R", 
    "aa03-ternaryplot-classes-utility.R", 
    "aa04-ternarysystems.R", 
    "aa05-ternarydata.R", 
    "aa06-ternary2xy.R", 
    "aa07-plotUtilities.R" 
)   

#   Find all the R files
allRFiles <- list.files( 
    path        = file.path( pkgDir, pkgName, "R" ), 
    pattern     = ".R", 
    ignore.case = TRUE, 
    full.names  = FALSE 
)   

allRFiles <- allRFiles[ !grepl( x = allRFiles, pattern = "R~", 
    fixed = TRUE ) ]

allRFiles <- allRFiles[ !(allRFiles %in% sourceFiles) ]

#   Find the dependencies in the description
desc <- utils::packageDescription(
    pkg     = pkgName, 
    lib.loc = pkgDir )  

findDeps <- function( d, what = c( "Depends", "Suggests", "Imports" ) ){
    return( unique( unlist( lapply( X = what, FUN = function(w){
        out <- d[[ w ]]
        # out <- gsub( x = out, pattern = w, replacement = "" ) 
        out <- gsub( x = out, pattern = "\n", replacement = "" ) 
        out <- gsub( x = out, pattern = " ", replacement = "" ) 
        out <- unlist( strsplit( x = out, split = "," )[[ 1L ]] )
        return( out[ !grepl( x = out, pattern = "R(>=", fixed = TRUE ) ] )
    } ) ) ) )
}   

(deps <- findDeps( d = desc ))

for( p in deps ){
    library( package = p, character.only = TRUE ) 
}   

for( f in sourceFiles ){
    source( file = file.path( pkgDir, pkgName, "R", f ) )
}   

for( f in allRFiles ){
    source( file = file.path( pkgDir, pkgName, "R", f ) )
}   
