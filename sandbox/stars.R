library(stars)
library(sf)

data(World)
data(land)
data(NLD_prov)

# Regular raster, multiple bands (4 bands, first two categorical)
tm_shape(land) + tm_raster()

# Warp to differnt projection
tm_shape(land, projection = "+proj=eck4") + tm_raster()


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

# Test warp and transform for small stars like this
r = st_set_crs(r, 4326)
r_sf <- st_as_sf(r)

rmerc = st_transform(r, 3857)
rmerc_sf = st_as_sf(rmerc)

tmap_mode("plot")

tm_shape(r) +
	tm_raster("m", style = "cat", palette = "cat") +
tm_shape(r_sf) +
	tm_borders(lwd = 3)

tm_shape(rmerc) +
	tm_raster("m", style = "cat", palette = "cat") +
tm_shape(rmerc_sf) +
	tm_borders(lwd = 3)

# 4326 to 3857 warp
tm_shape(r, raster.warp = TRUE) +
	tm_raster("m", style = "cat", palette = "cat") +
	tm_shape(rmerc_sf, is.master = TRUE) +
	tm_borders(lwd = 3)

# 4326 to 3857 transform
tm_shape(r, raster.warp = FALSE) +
	tm_raster("m", style = "cat", palette = "cat") +
	tm_shape(rmerc_sf, is.master = TRUE) +
	tm_borders(lwd = 3)

# view mode: 4326 to 3857 transform
tmap_mode("view")
tm_shape(r, raster.warp = FALSE) +
	tm_raster("m", style = "cat", palette = "cat") +
tm_shape(rmerc_sf) +
	tm_borders(lwd = 3)

# view mode: 4326 to 3857 warp
tm_shape(r, raster.warp = TRUE) +
	tm_raster("m", style = "cat", palette = "cat") +
tm_shape(rmerc_sf) +
	tm_borders(lwd = 3)



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

prec1 = dplyr::slice(prec, time, 1)
tm_shape(prec1) + tm_raster()

grd = st_as_stars(st_bbox(prec1), n = prod(dim(prec1)))

prec2 = st_warp(prec1, grd)


prec1_4 = dplyr::slice(prec, time, 1:4)

tm_shape(prec1_4) + tm_raster() # works but slow

tm_shape(prec) + tm_raster() + tm_facets(free.scales = FALSE) # even slower


# large stars object (10980 x 10980)
(p2 = read_stars(s2))

qtm(p2)

# smaller resolution
tmap_options(max.raster = c(plot = 1e3, view = 1e3))
qtm(p2)


# one band
p2_1 = dplyr::slice(p2, band, 1)

qtm(p2_1)


# one band, aspect ratio 2
p2_1b = p2_1[,1:5490]

qtm(p2_1b)


# warp to different crs
tm_shape(p2_1, projection = 4326) + tm_raster()

tmap_mode("view")
qtm(p2_1)



# slightly too large stars object (1100 x 1110)
tmap_options_reset()
(p3 = read_stars(s2, RasterIO = list(nXSize = 1100, nYSize = 1100)))

tm_shape(p3) +
	tm_raster()

tm_shape(p3, raster.downsample = FALSE) +
	tm_raster()

### spatialtemporal array
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
	st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
nc_outline = st_union(st_geometry(nc))
plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)

a = aggregate(prec, by = nc, FUN = max)
qtm(prec)

tm_shape(prec, raster.downsample = TRUE) + tm_raster(style = "kmean") # todo: raster.downsample depends on facets
qtm(a)



