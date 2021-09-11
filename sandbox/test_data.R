suppressPackageStartupMessages(library(pryr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(terra))


data(World, package = "tmap")
data(land, package = "tmap")
data(metro, package = "tmap")
data(rivers, package = "tmap")

prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
	st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
prec_nc = aggregate(prec, by = nc, FUN = max)

tif = system.file("tif/L7_ETMs.tif", package = "stars") %>% read_stars()
tif2 = c(tif,tif, along = 'testdim')

weather = read_ncdf(system.file('nc/bcsd_obs_1999.nc', package = 'stars'))
weather1 = st_set_dimensions(merge(weather), names = c('longitude','latitude','time','attributes'))
weather2 = split(weather1, 'time')

#install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")
#install.packages("~/Downloads/starsdata_0.0-1.tar.gz", repos = NULL, type = "source")
granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", package = "starsdata")
s2 = paste0("SENTINEL2_L1C:/vsizip/", granule, "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
#gran = read_stars(s2, proxy = FALSE)
gran_p = read_stars(s2, proxy = TRUE)

rm(granule, s2)
gc()

landsat_stars = read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
landsat = rast(system.file("raster/landsat.tif", package = "spDataLarge"))
lux <- vect(system.file("ex/lux.shp", package="terra"))



show_data = function() {
	require(dplyr)
	lst = ls(envir = .GlobalEnv)
	cls = sapply(lst, function(o) class(get(o, envir = .GlobalEnv))[1])
	size = sapply(lst, function(o) object_size(get(o, envir = .GlobalEnv)))
	features = sapply(lst, function(o) {
		o = get(o, envir = .GlobalEnv)
		if (inherits(o, "sf")) {
			paste0(ncol(o)-1, " cols x ",  nrow(o), " geoms")
		} else if (inherits(o, "stars")) {
			paste0(length(o), " attrs x (", paste(dim(o), collapse = " x "), ") dims")
		} else if (inherits(o, "SpatVector")) {
			paste0(dim(o)[2], " cols x ",  dim(o)[1], " geoms")
		} else if (inherits(o, "SpatRaster")) {
			paste0("(", paste(dim(o), collapse = " x "), ") dims")
		} else ""
	})
	hasr = sapply(lst, function(o) stars:::has_raster(get(o, envir = .GlobalEnv)))
	
	df = data.frame(name = names(size), size = format(unname(size), big.mark = ","),  class = cls, features = features, has_raster = hasr, row.names = NULL)
	df = df[df$class %in% c("sf", "stars", "SpatVector", "SpatRaster"),]
	
	df %>% 
	#	filter(class %in% c("sf", "stars")) %>% 
		arrange(class, desc(size)) %>% 
		print()
	
	cat("Memory used: ")
	print(mem_used())
}

show_data()


