# http://blog.revolutionanalytics.com/2009/11/choropleth-map-r-challenge.html
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

devtools::load_all(".")
# library(tmap)
library(raster)
library(rgeos)
library(ggplot2)
library(maps)
library(stringdist)
library(plyr)
library(scales)
library(RColorBrewer)

## read and crop shape 
zipfile = "../demo/shape/gz_2010_us_050_00_500k.zip"
zipdir <- tempdir()
unzip(zipfile, exdir=zipdir)

shp <-  read_shape(file = paste0(zipdir, "/gz_2010_us_050_00_500k.shp"))
shp$fips <- paste0(shp$STATE, shp$COUNTY)

data(state.fips)
shp$state_name <- as.character(state.fips$polyname[match(as.integer(as.character(shp$STATE)), state.fips$fips)])
shp$state_name <- sub(":.*", "", shp$state_name)
shp$mpname <- paste(tolower(shp$state_name), tolower(shp$NAME), sep=",")

shp <- crop_shape(shp, matrix(c(-130, -60, 25, 50), nrow = 2, byrow = TRUE))


## process data
unemp <- read.csv('../demo/data/US_unemployment09.csv',header=FALSE, stringsAsFactors=FALSE,
				  col.names=c("blsid", "stfips", "cofips", "name", "year", "pop1", "pop2", "unempraw", "unemppct"))

unemp$mpname <- tolower(paste(state.name[match(
	sub("^.*([A-Z][A-Z])$","\\1",unemp$name,fixed=FALSE),state.abb)],
	sub("^(.*) (County|[Cc]ity|Parish), ..$","\\1", unemp$name),sep=","))
unemp$mpname <- sub(":.*", "", unemp$mpname)
unemp$mpname <- sub(" county/(city|town),.*", "", unemp$mpname)
#unemp$mpname <- sub(" county/town,.*", "", unemp$mpname)


## match data to shp
(x <- setdiff(shp$mpname, unemp$mpname))
res <- amatch(x, unemp$mpname, method = "osa", weight = c(d = .001, i = 1, s = 1, t = .001), maxDist=10)

shp$mpname[match(x, shp$mpname)] <- unemp$mpname[res]

## append data to shp
shp <- append_data(unemp, shp, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)

qtm(shp, fill="unemppct", style="kmeans", palette="PuRd")



system.time({
	print(qtm(shp))
})

system.time({
	plot(shp)
})


## test ggplot2
shp.points <- fortify(shp, region="fips")
shp.points$fips <- shp.points$id
shp.df <- join(shp.points, shp@data, by="fips")

shp.df$unemppct_d <- cut(shp.df$unemppct, breaks = c(seq(0, 10, by = 2), 35))

system.time({
	print(ggplot(shp.df, aes(long, lat, group = group)) +
		geom_polygon(aes(fill = unemppct_d), colour = alpha("white", 1/2), size = 0.2) +
		#geom_polygon(data = state_df, colour = "white", fill = NA) +
		scale_fill_brewer(palette = "PuRd"))
})


system.time({
	print(tm_shape(shp) + tm_fill("unemppct", style="kmeans", palette="PuRd") + tm_borders("white"))
})


system.time({
	print(qtm(shp1))
})

system.time({
	plot(shp)
})

