set pkgname=ternaryplot

cd /D "%dropboxPath%\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

R CMD check %pkgname%

@REM --as-cran --no-tests 

pause
