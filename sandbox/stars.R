# Needs the stars branch of tmaptools:
# install_github("mtennekes/tmaptools", ref = "stars")

library(stars)
library(sf)

data(World)
data(land)
data(NLD_prov)

# Regular raster, multiple bands (4 bands, first two categorical)
tm_shape(land) + tm_raster()

# Regular raster, one band polygons overlay
tm_shape(land) + tm_raster("elevation") +
	tm_shape(World) +
	tm_borders()

# Cropped raster (view mode: just a different starting view)
tm_shape(land, bbox = "Germany") + 
	tm_raster("elevation")


# Regular grid without crs
m = matrix(1:20, nrow = 5, ncol = 4)
dim(m) = c(x = 5, y = 4) 
(s = st_as_stars(m))

tmap_mode("plot")
qtm(s)

tmap_mode("view")
qtm(s)


# Sheared / rotated grids
m = matrix(1:20, nrow = 5, ncol = 4)
dim(m) = c(x = 5, y = 4) 
(s = st_as_stars(m))

attr(s, "dimensions")[[2]]$delta = -1
attr(attr(s, "dimensions"), "raster")$affine = c(0.1, 0.1)

tmap_mode("plot")
qtm(s) # good

tmap_mode("view")
qtm(s) # Error: cannot allocate vector of size 1103.7 Gb 


# Rectilinear grids
m = matrix(1:20, nrow = 5, ncol = 4)
dim(m) = c(x = 5, y = 4) 

x = c(0, 0.5, 1, 2, 4, 5)  # 6 numbers: boundaries!
y = c(0.3, 0.5, 1, 2, 2.2) # 5 numbers: boundaries!
(r = st_as_stars(list(m = m), dimensions = st_dimensions(x = x, y = y)))

tmap_mode("plot")
qtm(r)

tmap_mode("view")
qtm(r) # different scale due to warp to 3857 (some matrix values got lost)

rsf <- st_as_sf(r)
tm_shape(rsf) +
	tm_polygons("m", style = "cat", palette = "cat")

tm_shape(r) +
	tm_raster(style = "cat", palette = "cat") +
tm_shape(rsf) +
	tm_borders()
