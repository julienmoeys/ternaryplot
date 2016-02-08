
pkgname="ternaryplot"
pkgVersion="0.6.7"

cd "/home/${USER}/Dropbox/_WORK/_PROJECTS/r_packages/${pkgname}/pkg"

R CMD Rd2pdf ${pkgname} --output="${pkgname}.${pkgVersion}.pdf"

read -p "Press [Enter] to quit this script"

