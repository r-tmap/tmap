# http://blog.revolutionanalytics.com/2009/11/choropleth-map-r-challenge.html
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

devtools::load_all(".")

zipfile = "../demo/shape/gz_2010_us_050_00_500k.zip"
zipdir <- tempdir()
unzip(zipfile, exdir=zipdir)


# library(tmap)

library(raster)
library(rgeos)

shp <-  read_shape(file = paste0(zipdir, "/gz_2010_us_050_00_500k.shp"))
shp$fips <- paste0(shp$STATE, shp$COUNTY)


shp <- crop_shape(shp, matrix(c(-130, -60, 25, 50), nrow = 2, byrow = TRUE))

# us_rect <- as(extent(-130, -60, 25, 50), "SpatialPolygons")
# us_rect <- set_projection(us_rect, current.projection = get_projection(shp))
# 
# shp <- append_data(as.data.frame(coordinates(shp)), shp, fixed.order = TRUE)
# shp <- shp[shp$V1> -130 & shp$V1 < -60 & shp$V2 > 25 & shp$V2 < 50, ]

shp1 <- shp[1,]

system.time({
	print(qplot(mpg, wt, data=mtcars))
})

system.time({
	print(qtm(shp))
})

system.time({
	plot(shp)
})





system.time({
	print(tm_shape(shp1) + tm_fill())
})



system.time({
	print(qtm(shp1))
})

system.time({
	plot(shp)
})


library(maps)
unemp <- read.csv('../demo/data/US_unemployment09.csv',header=FALSE, stringsAsFactors=FALSE,
				  col.names=c("blsid", "stfips", "cofips", "name", "year", "pop1", "pop2", "unempraw", "unemppct"))

unemp$mpname <- tolower(paste(state.name[match(
	sub("^.*([A-Z][A-Z])$","\\1",unemp$name,fixed=FALSE),state.abb)],
	sub("^(.*) (County|[Cc]ity|Parish), ..$","\\1", unemp$name),sep=","))
unemp$mpname <- sub(":.*", "", unemp$mpname)
unemp$mpname <- sub(" county/city,.*", "", unemp$mpname)
unemp$mpname <- sub(" county/town,.*", "", unemp$mpname)





mp <- map("county", plot=FALSE,namesonly=TRUE)

data(countyMapEnv)
data(state.fips)




data(county.fips)
str(county.fips)


setdiff(mp, unemp$mpname)
setdiff(unemp$mpname, mp)

shp$state_name <- as.character(state.fips$polyname[match(as.integer(as.character(shp$STATE)), state.fips$fips)])
shp$state_name <- sub(":.*", "", shp$state_name)

shp$mpname <- paste(tolower(shp$state_name), tolower(shp$NAME), sep=",")

setdiff(shp$mpname, county.fips$polyname)
setdiff(county.fips$polyname, shp$mpname)

x <- setdiff(shp$mpname, unemp$mpname)

require(stringdist)
res <- amatch(x, unemp$mpname, method = "osa", weight = c(d = .001, i = 1, s = 1, t = .001), maxDist=10)


shp$mpname[match(x, shp$mpname)] <- unemp$mpname[res]

setdiff(shp$mpname, unemp$mpname)

length(unique(shp$mpname))

shp <- append_data(unemp, shp, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)

qtm(shp, fill="unemppct", style="kmeans", palette="PuRd")

