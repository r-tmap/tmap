library(sf)
library(spData)
library(giscoR)
library(tmap)
	
sf_use_s2(FALSE)

# load 'broken' World dataset from tmap
data(World)

bb = st_bbox(giscoR::gisco_get_countries())
W2 = World |>
	st_transform(crs = 4326) |>
	st_intersection(st_as_sfc(bb))

sf_use_s2(TRUE)

isv = sf::st_is_valid(W2)
W2$name[!isv]

tm_shape(W2) + tm_polygons()

tm_shape(W2, projection = "+proj=eck4") + tm_polygons() # old default CRS for World

# round earth tests:
tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=0") + tm_polygons() 

tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=90") + tm_polygons()

tm_shape(W2, projection = "+proj=ortho +lon_0=0 +lat_0=45") + tm_polygons()

# valid? 
W2_ortho_0_0 = st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=0")
st_is_valid(W2_ortho_0_0)

W2_ortho_0_90 = st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=90")
st_is_valid(W2_ortho_0_90)

W2_ortho_0_45 = st_transform(W2, "+proj=ortho +lon_0=0 +lat_0=45")
st_is_valid(W2_ortho_0_45)

# s2plot works for 0 45:
library(s2plot)
s2plot::s2plot(W2, col = "grey90", projection = s2plot::s2plot_projection_orthographic("POINT (0 45)"))


st_geometry(W2) = st_cast(st_geometry(W2), "MULTIPOLYGON")


# overwrite and save
World = W2
save(World, file="data/World.rda", compress="xz")
