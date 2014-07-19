@echo off


echo Removing building information...
rm -rf output

md output
cd output
R CMD build --resave-data ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1
