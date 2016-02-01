set pkgname=ternaryplot

cd /D "C:\Users\%username%\Dropbox\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

REM svnversion > %pkgname%\REVISION
git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_VERSION

R CMD build --compact-vignettes="gs+qpdf" %pkgname% 

pause
