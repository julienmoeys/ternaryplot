set pkgname=ternaryplot

cd /D "%dropboxPath%\_WORK\_PROJECTS\r_packages\ternaryplot\pkg" 

prompt $g

@echo :::: Finding latest version of package source ::::

dir %pkgname%_*tar.gz /b /o:-n > tmp.txt
set /p targzfile=<tmp.txt 
del tmp.txt

@echo :::: Processing %targzfile% ::::

R CMD check %targzfile%

@REM --as-cran --no-tests 

pause
