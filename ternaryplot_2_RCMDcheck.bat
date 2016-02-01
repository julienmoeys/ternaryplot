set pkgname=ternaryplot

cd /D "C:\Users\%username%\Dropbox\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

R CMD check %pkgname%

@REM --as-cran --no-tests 

pause
