# http://blog.revolutionanalytics.com/2009/11/choropleth-map-r-challenge.html
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

devtools::load_all(".")
#library(tmap)
library(raster)
library(rgeos)
library(ggplot2)
library(maps)
library(stringdist)
library(plyr)
library(scales)
library(RColorBrewer)
library(maptools)
library(grid)

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

## get contiguous US
cont_cty <- crop_shape(shp_cty, matrix(c(-130, -60, 25, 50), nrow = 2, byrow = TRUE))
cont_cty <- set_projection(cont_cty, "+init=epsg:2163") # U.S. National Atlas Equal Area Projection
cont_st <- unionSpatialPolygons(cont_cty, cont_cty$state_name)

## get alaska
non_cont <- shp_cty[is.na(shp_cty$state_name),]
alaska <- crop_shape(non_cont, matrix(c(-180, -100, 40, 80), nrow = 2, byrow = TRUE), set.bbox = FALSE)

alaska <- set_projection(alaska, "+proj=longlat +ellps=GRS80 +datum=NAD83 +towgs84=-0.9956,1.9013,0.5215,-0.025915,-0.009246,-0.011599,-0.00062 +no_defs")

## get hawaii
hawaii <- crop_shape(non_cont, matrix(c(-160.5, -100, 0, 30), nrow = 2, byrow = TRUE), set.bbox = FALSE)

hawaii <- set_projection(hawaii, "+proj=tmerc +lat_0=21.66666666666667 +lon_0=-160.1666666666667 +k=1.000000 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")



## process data
unemp <- read.csv('../demo/data/US_unemployment09.csv',header=FALSE, stringsAsFactors=FALSE,
				  col.names=c("blsid", "stfips", "cofips", "name", "year", "pop1", "pop2", "unempraw", "unemppct"))

unemp$mpname <- tolower(paste(state.name[match(
	sub("^.*([A-Z][A-Z])$","\\1",unemp$name,fixed=FALSE),state.abb)],
	sub("^(.*) (County|[Cc]ity|Parish), ..$","\\1", unemp$name),sep=","))
unemp$mpname <- sub(":.*", "", unemp$mpname)
unemp$mpname <- sub(" county/(city|town),.*", "", unemp$mpname)
#unemp$mpname <- sub(" county/town,.*", "", unemp$mpname)


## append unemployment data to cont_cty
(x <- setdiff(cont_cty$mpname, unemp$mpname))
res <- amatch(x, unemp$mpname, method = "osa", weight = c(d = .001, i = 1, s = 1, t = .001), maxDist=10)
cont_cty$mpname[match(x, cont_cty$mpname)] <- unemp$mpname[res]
cont_cty <- append_data(cont_cty, unemp, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)



## append unemployment data to Alaska
alaska$mpname <- paste("alaska,", substr(alaska$mpname, 4, nchar(alaska$mpname)), ", ak", sep="")
(x2 <- setdiff(alaska$mpname, unemp$mpname[68:94]))
res2 <- amatch(substr(x2, 1, 20), substr(unemp$mpname[68:94],1,20), method = "osa", weight = c(d = .001, i = 1, s = 1, t = .001), maxDist=8)
alaska$mpname[match(x2, alaska$mpname)] <- unemp$mpname[68:94][res2]
alaska <- append_data(alaska, unemp, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)

## append unemployment data to Hawaii
hawaii$mpname <- gsub("NA", "hawaii", hawaii$mpname)
(x3 <-setdiff(hawaii$mpname, unemp$mpname))
hawaii <- hawaii[!(hawaii$mpname %in% x3), ]

hawaii <- append_data(hawaii, unemp, key.data = "mpname", key.shp = "mpname", ignore.duplicates = TRUE)


## first try
qtm(cont_cty, fill="unemppct", style="kmeans", palette="PuRd")




tm <- tm_shape(cont_cty) +
	tm_borders("white", alpha = .25, lwd= 1) +
	tm_fill("unemppct", style="fixed", breaks=c(seq(0, 10, by=2), 35), palette="PuRd", contrast = .9) +
tm_shape(cont_st) +
	tm_borders("white", lwd = 2) +
tm_layout(title="Unemployment", draw.frame=FALSE, bg.color = "gray90", outer.margins=0, title.cex = 1.5, legend.text.cex = 1.2)


# Alaska
tm_ak <- tm_shape(alaska) +
	tm_borders("white", alpha = .25, lwd= 1) +
	tm_fill("unemppct", style="fixed", breaks=c(seq(0, 10, by=2), 35), palette="PuRd", contrast = .9) +
	tm_layout(title="", bg.color = NA, draw.frame = FALSE, outer.margins=0, asp=0, legend.show=FALSE)

# Hawaii
tm_hi <- tm_shape(hawaii) +
	tm_borders("white", alpha = .25, lwd= 1) +
	tm_fill("unemppct", style="fixed", breaks=c(seq(0, 10, by=2), 35), palette="PuRd", contrast = .9) +
	tm_layout(title="", bg.color = NA, draw.frame = FALSE, outer.margins=0, asp=0, legend.show=FALSE)





png("../demo/US_unemp.png", width=1000, height=700)
print(tm)
dev.off()

png("../demo/US_unemp_small.png", width=545, height=382)
print(tm + tm_layout(scale=.6))
dev.off()

png("../demo/US_unemp_al_hi.png", width=1000, height=600)
print(tm + tm_layout(inner.margins=c(.02, .02, .02, .1), title.position=c("right", "bottom"), legend.position=c("right", "bottom"), legend.width = .15))
print(tm_ak, vp=viewport(x=.1, y=.13, width=.2, height=.27))
print(tm_hi, vp=viewport(x=.25, y=.075, width=.075, height=.1))
dev.off()

png("../demo/US_unemp_al_hi_small.png", width=545, height=328)
print(tm + tm_layout(inner.margins=c(.02, .02, .02, .1), title.position=c("right", "bottom"), legend.position=c("right", "bottom"), legend.width = .15, scale=.6))
print(tm_ak + tm_layout(scale=.6), vp=viewport(x=.1, y=.13, width=.2, height=.27))
print(tm_hi + tm_layout(scale=.6), vp=viewport(x=.25, y=.075, width=.075, height=.1))
dev.off()


# 
## test ggplot2
shp.points <- fortify(cont_cty, region="fips")
shp.points$fips <- shp.points$id
shp.df <- join(shp.points, cont_cty@data, by="fips")

shp.df$unemppct_d <- cut(shp.df$unemppct, breaks = c(seq(0, 10, by = 2), 35))

system.time({
	print(ggplot(shp.df, aes(long, lat, group = group)) +
		geom_polygon(aes(fill = unemppct_d), colour = alpha("white", 1/2), size = 0.2) +
		#geom_polygon(data = state_df, colour = "white", fill = NA) +
		scale_fill_brewer(palette = "PuRd"))
})


system.time({
	print(tm_shape(cont_cty) + tm_fill("unemppct", style="kmeans", palette="PuRd") + tm_borders("white"))
})


system.time({
	print(qtm(shp1))
})

system.time({
	plot(shp)
})

