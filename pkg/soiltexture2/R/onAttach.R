
# +-------------------------------------------------------------+
# | Package:    soiltexture2                                    |
# | Language:   R + roxygen2 inline documentation               |
# | Author(s):  Julien Moeys <Julien.Moeys@@slu.se>             |
# | License:    AGPL3, Affero General Public License version 3  |
# +-------------------------------------------------------------+

#   INTERNAL: set more package arguments that need to 
#       be set when the package is attached (because not 
#       available when defining the bulk of package arguments)
.setPackageArguments <- function( pkgname ){
    terSysEnvList  <- ternaryplot::getTpPar( "terSysEnvList" )
    terSysEnvList0 <- list( getAllTextureSystems )
    names( terSysEnvList0 ) <- pkgname 
    ternaryplot::tpPar( "terSysEnvList" = c(
        terSysEnvList0, 
        terSysEnvList
    ) ) 
}   

.onAttach <- function(# Internal. Message displayed when loading the package.
    libname, 
    pkgname  
){  
    .setPackageArguments( pkgname = pkgname )
    
    # Welcome message
    if( interactive() ){ 
        gitVersion <- system.file( "GIT_VERSION", package = pkgname ) 
        
        if( gitVersion != "" ){ 
            gitVersion <- readLines( con = gitVersion )[ 1L ] 
            gitVersion <- strsplit( x = gitVersion, split = " ", 
                fixed = TRUE )[[ 1L ]][ 1L ]
            
            gitVersion <- sprintf( "(git revision: %s)", gitVersion ) 
        }else{ 
            gitVersion <- "(git revision: ?)" 
        }   
        
        msg <- sprintf( 
            "%s %s %s. For help type: help(pack='%s')", 
            pkgname, 
            as.character( packageVersion( pkgname ) ), 
            gitVersion, # svnVersion
            pkgname ) 
        
        packageStartupMessage( msg ) 
    }   
}   
