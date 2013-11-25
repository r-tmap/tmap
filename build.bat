@echo off


echo Removing building information...
rm -rf output

echo Create map lookup database
R -q -f build/regions.R

echo Build documentation...
R -q -f build/roxygen.R

echo Build vignette...
echo R -q -f build/vignette.R

md output
cd output
R CMD build --resave-data ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1
