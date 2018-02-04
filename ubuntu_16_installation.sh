# install fundamental spatial libraries (needed for sf, sp, rgdal, rgeos)
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev 

# install v8, needed for the R package V8 which reverse imports geojsonio and rmapshaper -> tmaptools -> tmap
sudo apt-get install libv8-3.14-dev

# install jq, needed for the R package jqr whith reverse imports: geojson -> geojsonio -> rmapshaper -> tmaptools -> tmap
sudo add-apt-repository -y ppa:opencpu/jq
sudo apt-get update -q
sudo apt-get install -y libjq-dev

# install libraries needed for the R package protolite, which reverse imports: geojson -> geojsonio -> rmapshaper -> tmaptools -> tmap
sudo apt-get install -y libprotobuf-dev protobuf-compiler

# other libraries
sudo apt-get install libssl-dev
sudo apt-get install libcairo2-dev
