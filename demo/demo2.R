# http://blog.revolutionanalytics.com/2009/11/choropleth-map-r-challenge.html
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

devtools::load_all("./pkg")

zipfile = "../demo/shape/gz_2010_us_050_00_500k.zip"
zipdir <- tempdir()
unzip(zipfile, exdir=zipdir)


# library(tmap)

library(raster)
library(rgeos)

shp <-  read_shape(file = paste0(zipdir, "/gz_2010_us_050_00_500k.shp"))
shp$fips <- paste0(shp$STATE, shp$COUNTY)

us_rect <- as(extent(-130, -60, 25, 50), "SpatialPolygons")
us_rect <- set_projection(us_rect, current.projection = get_projection(shp))

shp <- append_data(as.data.frame(coordinates(shp)), shp, fixed.order = TRUE)

shp <- shp[shp$V1> -130 & shp$V1 < -60 & shp$V2 > 25 & shp$V2 < 50, ]

print(qtm(shp))
plot(shp)

q <- qtm(shp)

system.time({
	print(q)
})

system.time({
	plot(shp)
})



unemp <- read.csv('../demo/data/US_unemployment09.csv',header=FALSE, stringsAsFactors=FALSE,
				  col.names=c("blsid", "stfips", "cofips", "name", "year", "pop1", "pop2", "unempraw", "unemppct"))

unemp$fips <- substr(unemp$blsid,3,7)

setdiff(shp$fips, unemp$fips)
setdiff(unemp$fips, shp$fips)

shp$coverage <- shp$fips %in% unemp$fips

shp <- append_data(unemp, shp, key.data = "fips", key.shp = "fips", ignore.duplicates = TRUE)


qtm(shp, fill="coverage")
qtm(shp, fill="unemppct")

