library(devtools)
library(microbenchmark)
library(ggplot2)
library(sf)
library(stars)
library(mapview)
library(rnaturalearth)
library(spData)
library(raster)

options(viewer=NULL)

if (FALSE) {
	dev_mode()
	install_github("mtennekes/tmap") # most recent version (2020-04-02)
	
	dev_mode(path = "~/R-dev_old")
	install_github("mtennekes/tmap@895c4d4295") # version 2020-02-24 (3.0 before code improvements)
}




###### load datasets

# polygons
data("world")
w <- rnaturalearth::ne_download(scale = 10, type = 'countries', returnclass = "sf")


# rasters
granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", package = "starsdata")
s2 = paste0("SENTINEL2_L1C:/vsizip/", granule, "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
(p = read_stars(s2, proxy = TRUE))
p = st_set_crs(p, 32632)

(p2 = read_stars(s2, RasterIO = list(nXSize = 3000, nYSize = 3000)))
p2 = st_set_crs(p2, 32632)
r <- as(p2, "Raster")

prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))
prec1_4 = dplyr::slice(prec, time, 1:4)

data(land, package = "tmap")

rm(p2, prec); gc()



##############################
mb <- list()

######## references

# sf
mb$sf <- microbenchmark(times = 5,
			   plot(world[, "lifeExp"]),
			   plot(w[, "ECONOMY"]))

str(mb$sf)

dev.off(dev.list()["RStudioGD"])

# stars
mb$stars <- microbenchmark(times = 5,
							plot(p),
							plot(prec1_4))
dev.off(dev.list()["RStudioGD"])

# raster
mb$raster <- microbenchmark(times = 5,
						    plot(r),
							plot(land))
dev.off(dev.list()["RStudioGD"])

# mapview
mb$mapview <- microbenchmark(times = 3,
							 print(mapview(world, z = "lifeExp")),
							 print(mapview(w, z = "ECONOMY")),
							 print(mapview(r[[1]], homebutton = FALSE)))

######### tmap
mb_tmap = function(version) {
	if (version %in% c("cran_plot", "cran_view")) {
		microbenchmark(times = 3,
					   print(tm_shape(world) + tm_polygons("lifeExp") + tm_layout(title = version)),
					   print(tm_shape(w) + tm_polygons("ECONOMY")),
					   #print(tm_shape(p) + tm_raster()),
					   print(tm_shape(r) + tm_raster()),
					   #print(tm_shape(prec1_4) + tm_raster()),
					   print(tm_shape(land) + tm_raster()))
	} else {
		microbenchmark(times = 3,
					   print(tm_shape(world) + tm_polygons("lifeExp") + tm_layout(title = version)),
					   print(tm_shape(w) + tm_polygons("ECONOMY")),
					   print(tm_shape(p) + tm_raster()),
					   print(tm_shape(r) + tm_raster()),
					   print(tm_shape(prec1_4) + tm_raster()),
					   print(tm_shape(land) + tm_raster()))
	}
}


library(tmap)
tmap_mode("plot")
mb$tmap_CRAN_plot = mb_tmap(version = "cran_plot")
dev.off(dev.list()["RStudioGD"])

tmap_mode("view")
mb$tmap_CRAN_view = mb_tmap(version = "cran_view")

# gc()
# detach("package:tmap", unload = TRUE)
# dev_mode(TRUE, path = "~/R-dev_old")
# library(tmap)
# 
# tmap_mode("plot")
# mb$tmap_old_plot = mb_tmap(version = "old_plot")
# dev.off(dev.list()["RStudioGD"])
# 
# tmap_mode("view")
# mb$tmap_old_view = mb_tmap(version = "old_view")


gc()
detach("package:tmap", unload = TRUE)
# dev_mode(FALSE, path = "~/R-dev_old")
dev_mode(TRUE)
library(tmap)

tmap_mode("plot")
mb$tmap_new_plot = mb_tmap(version = "new_plot")
dev.off(dev.list()["RStudioGD"])

tmap_mode("view")
mb$tmap_new_view = mb_tmap(version = "new_view")

mbs = do.call(rbind, mb)




autoplot(mbs)
