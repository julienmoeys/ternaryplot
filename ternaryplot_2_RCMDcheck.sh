
pkgname="ternaryplot"
# pkgVersion="0.6.7"

cd "/home/${USER}/Dropbox/_WORK/_PROJECTS/r_packages/${pkgname}/pkg"

# R CMD check "${pkgname}_${pkgVersion}.tar.gz"
R CMD check ${pkgname}

read -p "Press [Enter] to quit this script"

