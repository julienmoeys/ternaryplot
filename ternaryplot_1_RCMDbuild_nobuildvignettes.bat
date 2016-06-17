set pkgname=ternaryplot

cd /D "%dropboxPath%\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_REVISION

R CMD build --no-build-vignettes %pkgname% 

pause
