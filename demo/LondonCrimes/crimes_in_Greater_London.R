library("sp")
library("rnaturalearth")

# functions to obtain crimes data
get_crimes_data <- function(path) {
	stopifnot(file.exists(path), ("crimes_in_Greater_London_2015-10.zip" %in% list.files(path)))
	tmp_dir <- tempdir()
	unzip(file.path(path, "crimes_in_Greater_London_2015-10.zip"), exdir=tmp_dir)
	rbind(read.csv(file.path(tmp_dir, "2015-10-city-of-london-street.csv")),
		  read.csv(file.path(tmp_dir, "2015-10-metropolitan-street.csv")))
}

# please download the file "crimes_in_Greater_London_2015-10.zip" (available on https://www.jstatsoft.org as a supplement of this paper), and change the path argument below to the location of the downloaded file:
download.file("http://www.von-tijn.nl/tijn/research/tmap/crimes_in_Greater_London_2015-10.zip", "crimes_in_Greater_London_2015-10.zip")
crimes <- get_crimes_data(path = ".")

# create SpatialPointsDataFrame of known locations
crimes <- crimes[!is.na(crimes$Longitude) & !is.na(crimes$Latitude), ]
coordinates(crimes) <- ~ Longitude + Latitude

# set map projection to British National Grid
crimes <- set_projection(crimes, current.projection = "longlat", projection = 27700)

# first glace at the data
tmap_mode("plot")
qtm(crimes)

# detecting outliers
crimes_osm <- read_osm(crimes)
qtm(crimes_osm) + qtm(crimes, symbols.col = "red", symbols.size = 0.5)

# show crime type of outliers
qtm(crimes_osm, raster.alpha = 0.5) + 
	qtm(crimes, symbols.col = "Crime.type", symbols.size = 0.5) + 
	tm_legend(outside = TRUE)

# download districts borders of Greater London
regions <- ne_download(scale = "large", type = "states", category = "cultural")
london <- regions[which(regions$region == "Greater London"),]
london <- set_projection(london, projection = 27700)

# remove crimes outside Greater London
crimes_london <- crop_shape(crimes, london, polygon =  TRUE)

# dot map with alpha transparency
qtm(crimes_london, dots.alpha = 0.1) +
	tm_shape(london) + 
	tm_borders()

# calculate kernal densities
crime_densities <- smooth_map(crimes_london, bandwidth = 0.5, breaks = c(0, 50, 100, 250, 500, 1000), cover = london)

# download rivers, and get Thames shape
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical")
thames <- crop_shape(rivers, london)

# plot kernel density map
tm_shape(crime_densities$polygons) +
	tm_fill(col = "level", palette = "YlOrRd", title = expression("Crimes per " * km^2)) + 
	tm_shape(london) + tm_borders() +
	tm_shape(thames) + tm_lines(col = "steelblue", lwd = 4) +
	tm_compass(position = c("left", "bottom")) +
	tm_scale_bar(position = c("left", "bottom")) + 
	tm_style_gray(title="Crimes in Greater London\nOctober 2015")

# select City of London
london_city <- london[london$name == "City",]
crimes_city <- crop_shape(crimes_london, london_city, polygon = TRUE)

# read OSM tiles of the City of London
london_osm <- read_osm(london_city, type = "stamen-watercolor", zoom = 13)

# small multiples of the City of London
qtm(london_osm) +
	qtm(crimes_city, dots.size = 0.2, by = "Crime.type", free.coords = FALSE)

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
crimes_city$Crime.group <- factor(crime_lookup[crimes_city$Crime.type], labels = crime_categories)

# interactive dot map of the City of London
tmap_mode("view")
tm_shape(crimes_city) +
	tm_dots(jitter = 0.2, col = "Crime.group", palette = "Dark2", popup.vars = TRUE) +
	tm_view(alpha = 1,
			basemaps = "Esri.WorldTopoMap")

# save the map to a stand-alone HTML page
save_tmap(filename = "index.html")
