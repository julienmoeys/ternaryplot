set pkgname=ternaryplot
set version=0.6.3

cd /D "C:\Users\%username%\Dropbox\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
