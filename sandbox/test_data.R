suppressPackageStartupMessages(library(pryr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(terra))


data(World, package = "tmap")
data(land, package = "tmap")
data(metro, package = "tmap")
data(World_rivers, package = "tmap")

prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = stars::read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
	sf::st_transform(sf::st_crs(prec)) -> nc # transform from NAD27 to WGS84
prec_nc = aggregate(prec, by = nc, FUN = max)

tif = system.file("tif/L7_ETMs.tif", package = "stars") %>% stars::read_stars()
tif2 = c(tif,tif, along = 'testdim')

weather = stars::read_ncdf(system.file('nc/bcsd_obs_1999.nc', package = 'stars'))
weather1 = stars::st_set_dimensions(merge(weather), names = c('longitude','latitude','time','attributes'))
weather2 = split(weather1, 'time')

#install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")
#install.packages("~/Downloads/starsdata_0.0-1.tar.gz", repos = NULL, type = "source")
granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", package = "starsdata")
s2 = paste0("SENTINEL2_L1C:/vsizip/", granule, "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
#gran = read_stars(s2, proxy = FALSE)
gran_p = stars::read_stars(s2, proxy = TRUE)

rm(granule, s2)
gc()

landsat_stars = read_stars(system.file("raster/landsat.tif", package = "spDataLarge"))
landsat_terra = rast(system.file("raster/landsat.tif", package = "spDataLarge"))

landsat_stars2 = split(landsat_stars)


lux <- vect(system.file("ex/lux.shp", package="terra"))

land_terra = terra::rast(methods::as(land, "Raster"))
ls_stars =  landsat_stars[,,,1,drop=TRUE]
ls_terra = terra::rast(methods::as(ls_stars, "Raster"))
names(ls_stars) = "layer"
la_stars = land[3]
la_terra = terra::rast(methods::as(la_stars, 'Raster'))
names(la_terra) = "trees"


World$gdp_est_mln = World$gdp_cap_est * World$pop_est / 1e6
World$well_being2 = round(World$well_being * rnorm(nrow(World), mean = 1, sd = .2), 1)
set.seed = 1234
World$r1 = round(runif(nrow(World), min = 0, max = 255))
World$g1 = round(runif(nrow(World), min = 0, max = 255))
World$b1 = round(runif(nrow(World), min = 0, max = 255))
World$r2 = round(pmin(pmax(World$r1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))
World$g2 = round(pmin(pmax(World$g1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))
World$b2 = round(pmin(pmax(World$b1 + rnorm(nrow(World), mean = 0, sd = 50), 0), 255))

#World$alpha_class = factor(floor(seq(1, 5, length.out = nrow(World) + 1)[1:nrow(World)]), labels = LETTERS[1:4])
World$pop_class = cut(World$pop_est, breaks = c(0, 10, 100, 1000, Inf) * 1e6, labels = c("Small", "Medium", "Large", "Extra Large"))					   

World$income_grp_int = as.integer(World$income_grp)
World$HPI2 = World$HPI / 2
World$HPI3 = round(World$HPI)

World$HPI_class = cut(World$HPI, breaks = seq(10, 50, by = 10))
World$well_being_class = cut(World$well_being, breaks = seq(2, 8, by = 2))
World$footprint_class = cut(World$footprint, breaks = seq(0, 16, by = 4))


#metro$alpha_class = factor(floor(seq(1, 5, length.out = nrow(metro) + 1)[1:nrow(metro)]), labels = LETTERS[1:4])


metro$pop2020_class = cut(metro$pop2020, breaks = c(.5, 1.5, 2.5, 5, 15, 40) * 1e6)

Africa = World[World$continent == "Africa", ]

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


