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

# Facets by
qtm(land) + tm_facets(by = "cover_cls") # still very slow


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
qtm(s)

tmap_mode("view")
qtm(s)


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


# tif rgb
tif = system.file("tif/L7_ETMs.tif", package = "stars")
rasterio = list(nXOff = 6, nYOff = 6, nXSize = 100, nYSize = 100, bands = c(1, 2, 3))
(x = read_stars(tif, RasterIO = rasterio))
dim(x)

tm_shape(x) +
	tm_rgb()


# stars proxy
granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", package = "starsdata")
s2 = paste0("SENTINEL2_L1C:/vsizip/", granule, "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
(p = read_stars(s2, proxy = TRUE))

qtm(p)

# stars time dimension
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

tm_shape(prec) + tm_raster() # works but 1) very slow 2) free scales should be off

tm_shape(prec) + tm_raster() + tm_facets(free.scales = FALSE) # even slower




