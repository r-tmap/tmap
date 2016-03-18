library("tmap")
library("sp")

# read crime data, downloaded from https://data.police.uk/data/
tmpdir <- tempdir()
unzip("crimes_in_Greater_London_2015-10.zip", exdir = tmpdir)
crimes <- rbind(read.csv(file.path(tmpdir, "2015-10-city-of-london-street.csv")),
				read.csv(file.path(tmpdir, "2015-10-metropolitan-street.csv")))

# create SpatialPointsDataFrame of known locations
crimes <- crimes[!is.na(crimes$Longitude)&!is.na(crimes$Latitude),]
coordinates(crimes) <- ~Longitude+Latitude
crimes <- set_projection(crimes, current.projection = "longlat", projection = 27700)

# download districts borders of Greater London
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip", destfile=file.path(tmpdir, "ne_10m_admin_1_states_provinces.zip"))
unzip(file.path(tmpdir, "ne_10m_admin_1_states_provinces.zip"), exdir = tmpdir)
regions <- read_shape(file.path(tmpdir, "ne_10m_admin_1_states_provinces.shp"))
london <- regions[which(regions$region == "Greater London"),]
london <- set_projection(london, projection = 27700)

# remove crimes outside Greater London
selection <- over(crimes, london)
crimes <- crimes[!is.na(selection$adm1_code), ]

# download rivers
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip", destfile=file.path(tmpdir, "ne_10m_rivers_lake_centerlines.zip"))
unzip(file.path(tmpdir, "ne_10m_rivers_lake_centerlines.zip"), exdir = tmpdir)
rivers <- read_shape(file.path(tmpdir, "ne_10m_rivers_lake_centerlines.shp"))
thames <- raster::crop(rivers,bb(london, projection = "longlat"))

# plot dot map
tm_shape(london) + tm_fill("white") + 
	tm_shape(crimes) + tm_dots(col = "orangered3", alpha = .1) + 
	tm_shape(london) + tm_borders() +
	tm_shape(thames) + tm_lines(col = "steelblue", lwd = 4) +
	tm_compass(position=c("left", "bottom")) +
	tm_scale_bar(position=c("left", "bottom")) + 
	tm_style_gray(title="Crimes in Greater London\nOctober 2015")

# quick dot map
qtm(crimes, dot.alpha = .1)

# dasymetric map
crime_densities <- smooth_map(crimes, bandwidth = .5, breaks = c(0, 50, 100, 250, 500, 1000), cover = london)

tm_shape(crime_densities$dasy) +
	tm_fill(col = "level", palette = "YlOrRd", title = "Crimes per km2") + 
	tm_shape(london) + tm_borders() +
	tm_shape(thames) + tm_lines(col = "steelblue", lwd = 4) +
	tm_compass(position = c("left", "bottom")) +
	tm_scale_bar(position = c("left", "bottom")) + tm_style_gray(title="Crimes in Greater London\nOctober 2015")

# select City of London
london_city <- london[london$name=="City",]
select_city <- over(crimes, london_city)
crimes_city <- crimes[!is.na(select_city$adm1_code), ]

# read OSM tiles of the City of London
london_bb <- bb(london_city, projection = "longlat")
london_osm <- read_osm(london_bb, type="stamen-watercolor", zoom=13)

# small multiples of the City of London
qtm(london_osm) +
	tm_shape(crimes_city) +
	tm_dots(size=.2) +
	tm_facets("Crime.type")

# group crime types
crime_lookup <- c('Anti-social behaviour'=2, 
				  'Bicycle theft'=1, 
				  'Burglary'=1,
				  'Criminal damage and arson'=2,
				  'Drugs'=6, 
				  'Other crime'=7,
				  'Other theft'=1, 
				  'Possession of weapons'=3, 
				  'Public order'=2, 
				  'Robbery'=1, 
				  'Shoplifting'=1,
				  'Theft from the person'=1,
				  'Vehicle crime'=4,
				  'Violence and sexual offences'=5)
crime_categories <- c("Property Crime",
					  "Criminal damage and anti-social behaviour",
					  "Possession of weapons",
					  "Vehicle crime",
					  "Violence and sexual offences",
					  "Drugs",
					  "Other crime")
crimes_city$Crime.group <- factor(crime_lookup[crimes_city$Crime.type], labels=crime_categories)

# interactive dot map of the City of London
tmap_mode("view")
tm_shape(crimes_city) +
	tm_dots(jitter = .2, col = "Crime.group", palette = "Dark2") +
	tm_view(alpha = 1,
			popup.all.data = TRUE,
			basemaps = "Esri.WorldTopoMap")
