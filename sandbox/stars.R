library(stars)
library(sf)

data(World)
data(land)
data(NLD_prov)


# Application: regular raster, multiple bands (4 bands, first two categorical)


tm_shape(land) + tm_raster()


# Application: regular raster, one band polygons overlay
tm_shape(land) + tm_raster("elevation") +
	tm_shape(World) +
	tm_borders()





# Application: cropped raster


tm_shape(land, bbox = "Germany") + 
	tm_raster("elevation")





+
tm_shape(NLD_prov, is.master = TRUE) +
	tm_borders()
