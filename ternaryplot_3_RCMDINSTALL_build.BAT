set pkgname=ternaryplot

cd /D "C:\Users\julienm\Dropbox\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

R CMD INSTALL --build --compact-docs --byte-compile %pkgname%

pause
