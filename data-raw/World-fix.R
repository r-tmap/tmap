library(sf)
library(spData)
library(giscoR)
library(tmap)
	
sf::sf_use_s2(FALSE)

# load 'broken' World dataset from tmap
data(World)

bb = sf::st_bbox(giscoR::gisco_get_countries())
W2 = World |>
	sf::st_transform(crs = 4326) |>
	sf::st_intersection(sf::st_as_sfc(bb))

sf::sf_use_s2(TRUE)

isv = sf::st_is_valid(W2)
W2$name[!isv]

tm_shape(W2) + tm_polygons()

tm_shape(W2, projection = "+proj=eck4") + tm_polygons() # old default CRS for World

# round earth tests:
tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=0") + tm_polygons() 

tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=90") + tm_polygons()

tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=45") + tm_polygons()

# valid? 
W2_ortho_0_0 = sf::st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=0")
sf::st_is_valid(W2_ortho_0_0)

W2_ortho_0_90 = sf::st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=90")
sf::st_is_valid(W2_ortho_0_90)

W2_ortho_0_45 = sf::st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=45")
sf::st_is_valid(W2_ortho_0_45)

# s2plot works for 0 45:
library(s2plot)
s2plot::s2plot(W2, col = "grey90", projection = s2plot::s2plot_projection_orthographic("POINT (0 45)"))


sf::st_geometry(W2) = sf::st_cast(sf::st_geometry(W2), "MULTIPOLYGON")


# overwrite and save
World = W2
save(World, file="data/World.rda", compress="xz")
