x <- read.table("../test/NDW_example/latlons.txt", sep=",", dec=".")

library(sp)

loops <- SpatialPointsDataFrame(coords=x[,3:2], data=x)
loops <- set_projection(loops, current.projection="longlat")

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, projection="longlat", current.projection="rd")

rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, projection="longlat", current.projection="rd")
rw$WEGNUMMER <- as.numeric(as.character(rw$WEGNUMMER))
rw_id <- as.numeric(get_IDs(rw))

loops_cr <- over(loops, corop)
loops$CR <- loops_cr$CR_2013

all.equal(as.integer(get_IDs(corop)) + 1, as.numeric(corop$CR_2013))

library(rgeos)
y <- gIntersection(rw, corop, byid=TRUE)


y_id <- get_IDs(y)
y_spl <- strsplit(y_id, split=" ", fixed=TRUE)
y_rw <- as.numeric(as.character(sapply(y_spl, function(x)x[1])))
y_cr <- as.numeric(sapply(y_spl, function(x)x[2])) + 1
y_len <- SpatialLinesLengths(y, longlat=TRUE)
ydata <- data.frame(ID=y_id, rw=y_rw, cr=y_cr, len=y_len)
y <- SpatialLinesDataFrame(y, ydata, match.ID=FALSE)
y$wn <- rw$WEGNUMMER[match(y$rw, rw_id)]

tab <- tapply(y$len, INDEX=list(y$wn, y$cr), sum)
tab[is.na(tab)] <- 0

sum(tab)
5.121

rowSums(tab)


geo_shape(corop) +
	geo_borders() +
geo_shape(loops) +
	geo_bubblemap(col="CR", size=.4, scale=.5)


table(loops$CR)

## test different method
# which.max(SpatialLinesLengths(y, longlat=TRUE) - SpatialLinesLengths(y, longlat=FALSE)) # returns 399 = wn 6 = A6
y@data[399,]



## plot A2
plot(corop)
plot(rw[rw$WEGNUMMER==2,], lwd=5, col="blue", add=TRUE)

plot(y[y$wn==2 & y$cr==38,], lwd=5, col="red", add=TRUE)

## opgeblazen (in werkelijkheid 217 km)
sum(tab[2,]) / 217

## A1 = 157 km
sum(tab[1,]) / 157

## A6 = 101 km
sum(tab[6,]) / 101
