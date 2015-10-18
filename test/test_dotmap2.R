# download Dutch neighborhood shape file
dir <- tempdir()
temp <- tempfile()

download.file("http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/links/2014-buurtkaart-shape-versie-1-el.htm", temp, mode="wb")
unzip(temp, exdir = dir)

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
	
	P_NLD <- 100 - P_WEST_AL - P_N_W_AL
	
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

# Sample dots (each dot represents 10 persons)
DH_nbhd_dots2 <- sample_dots(DH_nbhd, c("dutch", "west", "non_west"), convert2density = FALSE, N=750000, w=10, var.labels = c("Dutch (native)", "Western immigrants", "Non-western immigrants"), shp.id = "ID")



########################################################
## Choropleth of population densities per group
########################################################

tm_shape(DH_nbhd) +
	tm_polygons(c("dutch", "west", "non_west"), 
				palette=list("Greens", "Oranges", "Purples"), 
				breaks=c(0, 500, 1000, 1500, 2500, 5000)) +
tm_layout(inner.margins=0)


########################################################
## Choropleth of percentages 
########################################################

tm_shape(DH_nbhd) +
	tm_polygons(c("P_NLD", "P_WEST_AL", "P_N_W_AL"), 
				palette=list("Greens", "Oranges", "Purples"), 
				breaks=seq(0, 100, by=20)) +
tm_layout(inner.margins=0)

########################################################
## Dotmap of population
########################################################

## each dot represents 100 people
tm_shape(DH_nbhd_dots) + 
	tm_dots("class", size=.04, alpha=.75,
			palette="Dark2", title = "The Hague population") +
tm_layout(inner.margins=0, bg.color="white")


## with Open Streetmap layer
tm_shape(DH_nbhd_osm) + tm_raster(saturation=0) +
tm_shape(DH_nbhd_dots) + 
	tm_dots("class", size=.04, alpha=.75,
			palette="Dark2", title = "The Hague population") +
	tm_layout(inner.margins=0, bg.color="white")


## each dot represents 10 people
tm_shape(DH_nbhd_dots2) + 
	tm_dots("class", size=.02, alpha=.75, 
			palette="Dark2", title = "The Hague population") +
tm_layout(inner.margins=0, bg.color="white")


########################################################
## Links
########################################################

# http://demographics.coopercenter.org/DotMap/
# http://research.cbs.nl/dotmap/
