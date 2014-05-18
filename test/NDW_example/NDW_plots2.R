library(sp)
library(rgeos)

x <- read.table("../test/NDW_example/latlons.txt", sep=",", dec=".")


utm31 <- "+proj=utm +zone=31U +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


loops <- SpatialPointsDataFrame(coords=x[,3:2], data=x)
loops <- set_projection(loops, current.projection="longlat", projection=utm31)

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, projection=utm31, current.projection="rd")

loops_cr <- over(loops, corop)
loops$CR <- loops_cr$CR_2013
all.equal(as.integer(get_IDs(corop)) + 1, as.numeric(corop$CR_2013))



rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd", projection=utm31)
rw$WEGNUMMER <- as.numeric(as.character(rw$WEGNUMMER))
rw_id <- as.numeric(get_IDs(rw))

#### load all roads in the Netherlands (LARGE!!)
#nwb <- get_shape("../../GIS/nwb2013/nwb2013.shp")
#save(nwb, file="../../GIS/nwb2013/nwb2013.rdata")
#load(file="../../GIS/nwb2013/nwb2013.rdata")

## calculate total lengths of the roads
rw_len <- SpatialLinesLengths(rw, longlat=TRUE)
tw_dist <- tapply(rw_len, INDEX=list(rw$WEGNUMMER), sum)
real_dist <- c(157, 217, 9.7, 116, 17, 101, 242, 9.9, 96, 32) #from Wiki: quality unknown...
plot(tw_dist[1:10] / real_dist)



rw_cr <- gIntersection(rw, corop, byid=TRUE)


y_id <- get_IDs(rw_cr)
y_spl <- strsplit(y_id, split=" ", fixed=TRUE)
y_rw <- as.numeric(as.character(sapply(y_spl, function(x)x[1])))
y_cr <- as.numeric(sapply(y_spl, function(x)x[2])) + 1
y_len <- SpatialLinesLengths(rw_cr, longlat=TRUE)
ydata <- data.frame(ID=y_id, rw=y_rw, cr=y_cr, len=y_len)
rw_cr <- SpatialLinesDataFrame(rw_cr, ydata, match.ID=FALSE)
rw_cr$wn <- rw$WEGNUMMER[match(rw_cr$rw, rw_id)]

tab <- tapply(rw_cr$len, INDEX=list(rw_cr$wn, rw_cr$cr), sum)
tab[is.na(tab)] <- 0

tab2 <- tab / sum(tab) * 5121 / 2

## total road distances
rowSums(tab2)

tab2df <- cbind(data.frame(wegnr=row.names(tab), total=rowSums(tab2)), as.data.frame(tab2))
names(tab2df)[-c(1,2)] <- paste0("CR", names(tab2df)[-c(1,2)])


write.table(tab2df, file="../test/NDW_example/road_lengths.txt", sep=",", row.names=FALSE)



pdf("../test/NDW_example/loops.pdf", height=8, width=8)
geo_shape(corop) +
	geo_fill(col="CR_2013", palette="Pastel2") +
	geo_borders(col="grey50", lwd=.75) +
geo_shape(rw) +
	geo_lines(col="grey30", width=3) +
geo_shape(loops) +
	geo_bubbles(col="CR", size=.1, scale=1) + 
geo_theme_NLD(legend.profile="hide", title="Rijkswegen en lussen per corop")
dev.off()


pdf("../test/NDW_example/loops2.pdf", height=8, width=8)
geo_shape(corop) +
	geo_fill(col="CR_2013", palette="Pastel2") +
	geo_borders(col="grey50", lwd=.75) +
geo_shape(rw) +
	geo_lines(col="grey30", width=.5) + 
geo_theme_NLD(legend.profile="hide", title="Rijkswegen")
dev.off()

table(loops$CR)

## test different method
# which.max(SpatialLinesLengths(y, longlat=TRUE) - SpatialLinesLengths(y, longlat=FALSE)) # returns 399 = wn 6 = A6
y@data[399,]



## plot A2
plot(corop)
plot(rw[which(rw$WEGNUMMER==2),], lwd=5, col="red", add=TRUE)
plot(rw_cr[which(rw_cr$wn==2),], lwd=5, col="blue", add=TRUE)

plot(y[y$wn==2 & y$cr==38,], lwd=5, col="red", add=TRUE)

## opgeblazen (in werkelijkheid 217 km)
sum(tab[2,]) / 217

## A1 = 157 km
sum(tab[1,]) / 157

## A6 = 101 km
sum(tab[6,]) / 101
