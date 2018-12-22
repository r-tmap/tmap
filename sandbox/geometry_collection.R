library(sf)
library(lwgeom)
library(dplyr)

gc1 <- st_geometrycollection(list(st_linestring(rbind(c(0,0),c(1,1),c(2,1))), st_point(5:6)))

st_collection_extract(gc1, "POLYGON")
st_collection_extract(gc1, "POINT")
st_collection_extract(gc1, "LINESTRING")

x <- read_sf("Constituency_Boundaries__Generalised_20m__OSi_National_Statuatory_Boundaries_.geojson")

x2 <- tmaptools::simplify_shape(x)
#x3 <- st_make_valid(x2)

qtm(x2)

plot(x2)


tm_shape(x2) + 
	tm_polygons() + 
	tm_lines("red")

qtm(x2)
qtm(x2[1:6,])

tm_shape(x2) + tm_basemap() + tm_borders() + tm_fill() + tm_tiles("OpenStreetMap") + tm_layout()