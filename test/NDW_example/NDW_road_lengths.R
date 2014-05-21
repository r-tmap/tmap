devtools::load_all()
library(sp)
library(rgeos)

loopsdata <- read.csv("../test/NDW_example/Rijkswegen_loops.csv")
loopsdata$ROADNUMBER <- as.character(loopsdata$ROADNUMBER)
roadnumber <- unique(loopsdata$Rijksweg)
roadname <- loopsdata$ROADNUMBER[match(roadnumber, loopsdata$Rijksweg)]
loopsdata$ROADNUMBER <- factor(loopsdata$ROADNUMBER, levels=roadname)
	
	
loops <- SpatialPointsDataFrame(coords=loopsdata[,3:2], data=loopsdata)
loops <- set_projection(loops, current.projection="longlat", projection="rd")


corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

# loops_cr <- over(loops, corop)
# loops$CR <- loops_cr$CR_2013
# all.equal(as.integer(get_IDs(corop)) + 1, as.numeric(corop$CR_2013))

rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(roadname[match(rw$roadnumber, roadnumber)], levels=roadname)


geo_shape(corop) +
	geo_fill() +
geo_shape(rw) +
	geo_lines(col="roadname", by=TRUE)


## case study: A2
# volgens Google Maps 216 km van A'dam tot grens met Belgie

rwA2 <- rw[rw$roadname=="A2",]
geo_shape(corop) +
	geo_borders() +
	geo_fill() +
geo_shape(rwA2) +
	geo_lines() +
	geo_theme("A2")

loopsA2 <- loops[which(loops$ROADNUMBER=="A2"), ]


get_polygon_ranges(corop)$total.range
A2_len <- SpatialLinesLengths(rwA2, longlat=FALSE)
sum(A2_len) / 1000 # lengte A2 in km

A2_seglengths <- sapply(1:length(rwA2), function(i) {
	SpatialLinesLengths(rwA2[i,], longlat=FALSE)	
}) / 1000


# rwA2$id <- order(A2_seglengths, decreasing=TRUE)
# rwA2$id[rwA2$id>5] <- 5

pdf("../test/NDW_example/A2.pdf", width=8, height=8)
geo_shape(corop) +
	geo_borders() +
	geo_fill() +
geo_shape(rwA2) +
	geo_lines(col="red", lwd=.01) +
geo_shape(loopsA2) +
	geo_bubbles(col="steelblue", size=.01) +
	geo_theme("A2")
dev.off()


A2coor <- lapply(rwA2@lines, function(x) {
	coor <- lapply(x@Lines, function(y)y@coords)
	do.call("rbind", coor)
})
A2coor <- as.data.frame(do.call("rbind", A2coor))


A2line <- loess(V1~V2, data=A2coor)

plot(A2coor)
plot(A2line, col= "red", lwd=.5, add=TRUE)
