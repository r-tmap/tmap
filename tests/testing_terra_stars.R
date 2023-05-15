

library(tmap)
library(terra)
library(stars)
library(spDataLarge)

tif_stars = system.file("tif/L7_ETMs.tif", package = "stars") %>% read_stars()
tif_terra = system.file("tif/L7_ETMs.tif", package = "stars") %>% rast()

landsat_stars = read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
landsat_terra = rast(system.file("raster/landsat.tif", package = "spDataLarge"))

land_stars = land
land_terra = rast(land)
names(land_terra) = names(land) # bug in terra?

tm_shape(land_stars) + tm_raster("trees")
tm_shape(land_stars) + tm_raster("treess")

tm_shape(land_terra) + tm_raster("trees")
tm_shape(land_terra) + tm_raster("treess")


tm_shape(landsat_stars) + tm_raster("landsat.tif", col.free = FALSE)
tm_shape(landsat_terra) + tm_raster("landsat_1", col.free = FALSE)

## defaults are identical:
# (terra uses integers, so tm_scale_discrete is used)
tm_shape(landsat_stars) + tm_raster(col.free = FALSE)
tm_shape(landsat_stars) +
	tm_raster("landsat.tif") +
	tm_facets("band") + tm_options(max.raster = 10000)
tm_shape(landsat_terra) + tm_raster(col.free = FALSE)
tm_shape(landsat_terra) +
	tm_raster(c("landsat_1", "landsat_2", "landsat_3", "landsat_4"), col.free = FALSE) + tm_options(max.raster = 10000)


tm_shape(landsat_terra) +
	tm_raster(c("lan_1", "lan_2", "lan_3", "lan_4"), col.free = FALSE)

tm_shape(tif_terra) + tm_rgb()
tm_shape(tif_stars) + tm_rgb()


tm_shape(landsat_terra) +
	tm_rgb(tm_mv("landsat_4", "landsat_3", "landsat_2"), col.scale = tm_scale_rgb(maxValue = 31961))

tm_shape(landsat_stars) +
	tm_rgb(tm_mv(3,2,1), col.scale = tm_scale_rgb(maxValue = 31961))






