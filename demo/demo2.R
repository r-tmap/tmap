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
library(maptools)

## read and crop shape 
zipfile = "../demo/shape/gz_2010_us_050_00_500k.zip"
zipdir <- tempdir()
unzip(zipfile, exdir=zipdir)

shp_cty <-  read_shape(file = paste0(zipdir, "/gz_2010_us_050_00_500k.shp"))
shp_cty$fips <- paste0(shp_cty$STATE, shp_cty$COUNTY)

data(state.fips)
shp_cty$state_name <- as.character(state.fips$polyname[match(as.integer(as.character(shp_cty$STATE)), state.fips$fips)])
shp_cty$state_name <- sub(":.*", "", shp_cty$state_name)
shp_cty$mpname <- paste(tolower(shp_cty$state_name), tolower(shp_cty$NAME), sep=",")

shp_cty <- crop_shape(shp_cty, matrix(c(-130, -60, 25, 50), nrow = 2, byrow = TRUE))


## process data
unemp <- read.csv('../demo/data/US_unemployment09.csv',header=FALSE, stringsAsFactors=FALSE,
				  col.names=c("blsid", "stfips", "cofips", "name", "year", "pop1", "pop2", "unempraw", "unemppct"))

unemp$mpname <- tolower(paste(state.name[match(
	sub("^.*([A-Z][A-Z])$","\\1",unemp$name,fixed=FALSE),state.abb)],
	sub("^(.*) (County|[Cc]ity|Parish), ..$","\\1", unemp$name),sep=","))
unemp$mpname <- sub(":.*", "", unemp$mpname)
unemp$mpname <- sub(" county/(city|town),.*", "", unemp$mpname)
#unemp$mpname <- sub(" county/town,.*", "", unemp$mpname)


## match data to shp_cty
(x <- setdiff(shp_cty$mpname, unemp$mpname))
res <- amatch(x, unemp$mpname, method = "osa", weight = c(d = .001, i = 1, s = 1, t = .001), maxDist=10)

shp_cty$mpname[match(x, shp_cty$mpname)] <- unemp$mpname[res]

## append data to shp_cty
shp_cty <- append_data(unemp, shp_cty, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)

## first try
qtm(shp_cty, fill="unemppct", style="kmeans", palette="PuRd")


## polishing
# U.S. National Atlas Equal Area Projection

shp_st <- unionSpatialPolygons(shp_cty, shp_cty$state_name)

tm <- tm_shape(shp_cty, projection = "+init=epsg:2163") +
	tm_borders("gray50", lwd= 1) +
	tm_fill("unemppct", style="fixed", breaks=c(seq(0, 10, by=2), 35), palette="PuRd", contrast = .9) +
tm_shape(shp_st) +
	tm_borders("white", lwd = 2) +
tm_layout(title="Unemployment", bg.color = "white", draw.frame = TRUE, outer.margins=0, asp=0, legend.title.cex = 2, legend.text.cex = 1.2)

png("../demo/US_unemp.png", width=1000, height=700)
print(tm)
dev.off()



# 
# 
# 
## test ggplot2
shp.points <- fortify(shp_cty, region="fips")
shp.points$fips <- shp.points$id
shp.df <- join(shp.points, shp_cty@data, by="fips")

shp.df$unemppct_d <- cut(shp.df$unemppct, breaks = c(seq(0, 10, by = 2), 35))

system.time({
	print(ggplot(shp.df, aes(long, lat, group = group)) +
		geom_polygon(aes(fill = unemppct_d), colour = alpha("white", 1/2), size = 0.2) +
		#geom_polygon(data = state_df, colour = "white", fill = NA) +
		scale_fill_brewer(palette = "PuRd"))
})


system.time({
	print(tm_shape(shp_cty) + tm_fill("unemppct", style="kmeans", palette="PuRd") + tm_borders("white"))
})


system.time({
	print(qtm(shp1))
})

system.time({
	plot(shp)
})

