set pkgname=soiltexture2

cd /D "%dropboxPath%\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_VERSION

R CMD build %pkgname% 

pause
