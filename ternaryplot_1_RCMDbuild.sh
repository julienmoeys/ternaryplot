
pkgname="ternaryplot"

cd "/home/${USER}/Dropbox/_WORK/_PROJECTS/r_packages/${pkgname}/pkg"

git log -n 1 --oneline --no-notes > ${pkgname}/inst/GIT_VERSION

R CMD build --compact-vignettes ${pkgname} 

read -p "Press [Enter] to quit this script"

