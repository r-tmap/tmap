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

# Application: cropped raster (view mode: just a different starting view)
tm_shape(land, bbox = "Germany") + 
	tm_raster("elevation")


# Application: Rectilinear grids
m = matrix(1:20, nrow = 5, ncol = 4)
dim(m) = c(x = 5, y = 4) 

x = c(0, 0.5, 1, 2, 4, 5)  # 6 numbers: boundaries!
y = c(0.3, 0.5, 1, 2, 2.2) # 5 numbers: boundaries!
(r = st_as_stars(list(m = m), dimensions = st_dimensions(x = x, y = y)))





ttm()
qtm(r2)

tm_shape(r) +
	tm_raster()

ttm()

