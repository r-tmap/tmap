

library(tmap)
library(terra)
library(stars)
library(spDataLarge)

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
	tm_rgb(tm_mv("landsat_4", "landsat_3", "landsat_2"), col.scale = tm_scale_rgb(maxValue = 31961))




# approach: reshape landsat_stars (somewhere in the shape functions)
# - step1_helper L75: create value
# - subset: use this to reshape stars stars::split
# - Meta2: redo smeta
landsat_stars2

# step2_helper L292 move ShapeMeta1 redo earlier, so that faceting will be succesfull

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)

tm_shape(multi_rast) +
	tm_rgb(tm_mv("landsat_4", "landsat_3", "landsat_2"), col.scale = tm_scale_rgb(maxValue = 31961)) 
#> Error: [subset] no (valid) layer selected

tm_shape(multi_rast[[3:1]]) +
	tm_rgb() 


# idea: tm_attr to specify an attribute as mv





######
library(spDataLarge)
library(stars)

# direct approach
landsat_stars = read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
tm_shape(landsat_stars) +
	tm_rgb(tm_mv_dim("band", c(4,3,2)), col.scale = tm_scale_rgb(maxValue = 31961))

# indirect approach
landsat_stars2 = landsat_stars |> split("band")
tm_shape(landsat_stars2) +
	tm_rgb(tm_mv("X4", "X3", "X2"), col.scale = tm_scale_rgb(maxValue = 31961))
