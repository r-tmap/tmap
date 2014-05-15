x <- read.table("../test/NDW_example/latlons.txt", sep=",", dec=".")

library(sp)

loops <- SpatialPointsDataFrame(coords=x[,3:2], data=x)

loops <- set_projection(loops, current.projection="longlat")

corop <- get_shape("../test/NDW_example/cr_2013.shp")

corop <- set_projection(corop, projection="longlat", current.projection="rd")

# nwb <- get_shape("../../GIS/nwb2013/nwb2013.shp")
# 
# save(nwb, file="../../GIS/nwb2013/nwb2013.rdata")
# load(file="../../GIS/nwb2013/nwb2013.rdata")


rw <- get_shape("../../GIS/nwb2013/rijksweg2013.shp")

plot(rw)

?overlay

corop <- set_projection(corop, projection="rd")

rw@proj4string <- CRS(proj4string(loops))
corop@proj4string

y <- over(corop, rw)
loops_cr <- over(loops, corop)

loops$CR <- loops_cr$CR_2013


corop@data[2,]

geo_shape(corop) +
	geo_borders() +
geo_shape(loops) +
	geo_bubblemap(col="CR", size=.4, scale=.5)


table(loops$CR)

