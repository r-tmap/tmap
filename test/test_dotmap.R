data(World)
World_dots <- sample_dots(World, vars="pop_est_dens", nrow=200, ncol=400, w=1e6)

tm_shape(World_dots) + tm_dots(size = .02, jitter=.1) + tm_layout("One dot represents one million people", title.position = c("right", "bottom"))

# download Dutch neighborhood shape file
dir <- tempdir()
temp <- tempfile()

download.file("http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/links/2014-buurtkaart-shape-versie-1-el.htm", temp, mode="wb")
unzip(temp, exdir = dir)

#NLD_nbhd <- read_shape("../../shape_files/buurt_2014.shp")
NLD_nbhd <- read_shape(file.path(dir, "buurt_2014.shp"))

# fix self-intersection but in shape object
NLD_nbhd <- rgeos::gBuffer(NLD_nbhd, byid=TRUE, width=0)

# remove all water neighborhoods
NLD_nbhd <- NLD_nbhd[NLD_nbhd$OPP_LAND>0, ]

# process data
NLD_nbhd@data <- within(NLD_nbhd@data, {
	# convert land area ("OPP_LAND") from hectare (ha) to km2
	OPP_LAND <- OPP_LAND / 100

	# set negative percentages of western and non-western immigrants to 0
	P_WEST_AL[P_WEST_AL<0] <- 0
	P_N_W_AL[P_N_W_AL<0] <- 0
	
	# calculate population density values
	dens <- (AANT_INW / OPP_LAND)
	dens[is.nan(dens)] <- 0
	
	# divide population in three groups: western and non-western immigrants, and native Dutch
	west <- dens * P_WEST_AL / 100
	non_west <- dens * P_N_W_AL / 100
	dutch <- dens - non_west - west
})

# Select The Hague (Den Haag) area
DH_bbox <- bb(NLD_nbhd[which(NLD_nbhd$GM_NAAM=="'s-Gravenhage"), ])

# Crop shape to The Hague area (use 1.05 bounding box extension to make sure the whole region is covered after reprojection to the Mercator porjection)
DH_nbhd <- raster::crop(NLD_nbhd, bb(DH_bbox, 1.05))

# Read OSM layer
DH_nbhd_osm <- read_osm(bb(DH_bbox, current.projection="rd", projection="longlat"), type = "mapquest")

# Sample dots (each dot represents 100 persons)
DH_nbhd_dots <- sample_dots(DH_nbhd, c("dutch", "west", "non_west"), convert2density = FALSE, N=250000, w=100, var.labels = c("Dutch (native)", "Western immigrants", "Non-western immigrants"), shp.id = "ID")


# Show map
tm_shape(DH_nbhd_osm) + tm_raster(saturation=.2) +
	tm_shape(DH_nbhd_dots) + tm_dots("class", size=.04, alpha=.75, palette="Dark2", title = "The Hague population") +
	tm_layout(inner.margins=0, legend.frame=TRUE, legend.bg.color="grey90")




# 
# 
# 
# png("dotmap2c2.png", width=DH_nbhd_osm@grid@cells.dim[1], height=DH_nbhd_osm@grid@cells.dim[2])
# tm_shape(DH_nbhd_osm) + tm_raster(saturation=.2) +
# 	tm_shape(DH_nbhd_dots) + tm_dots("class", size=.04, alpha=.75, palette="Dark2", title = "The Hague population") +
# 	tm_layout(inner.margins=0, legend.frame=TRUE, legend.bg.color="grey90", outer.margins=0, scale=1.5)
# dev.off()


# Animation of 10 plots

DH_nbhd_dots_ani <- lapply(1:10, function(i) {
	sample_dots(DH_nbhd, c("dutch", "west", "non_west"), convert2density = FALSE, N=250000, w=100, var.labels = c("Dutch (native)", "Western immigrants", "Non-western immigrants"), shp.id = "ID")
})

animation_tmap({
	for (i in 1:10) {
		print(tm_shape(DH_nbhd_osm) + tm_raster(saturation=.2) +
			tm_shape(DH_nbhd_dots_ani[[i]]) + tm_dots("class", size=.04, alpha=.75, palette="Dark2", title = "The Hague population") +
			tm_layout(inner.margins=0, legend.frame=TRUE, legend.bg.color="grey90", outer.margins=0, scale=1.5))
	}
}, filename = "dotmap_animation.gif", width=DH_nbhd_osm@grid@cells.dim[1], height=DH_nbhd_osm@grid@cells.dim[2])


# Sample dots (each dot represents 10 persons)
DH_nbhd_dots2 <- sample_dots(DH_nbhd, c("dutch", "west", "non_west"), convert2density = FALSE, N=250000, w=10, var.labels = c("Dutch (native)", "Western immigrants", "Non-western immigrants"), shp.id = "ID")

png("dotmap3.png", width=DH_nbhd_osm@grid@cells.dim[1]*2, height=DH_nbhd_osm@grid@cells.dim[2]*2)
tm_shape(DH_nbhd_osm) + tm_raster(saturation=.2) +
	tm_shape(DH_nbhd_dots2) + tm_dots("class", size=.04, alpha=.75, jitter=.1, palette="Dark2", title = "The Hague population") +
	tm_layout(inner.margins=0, legend.frame=TRUE, legend.bg.color="grey90", outer.margins=0, scale=1.5)
dev.off()
