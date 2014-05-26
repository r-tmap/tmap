devtools::load_all()
library(sp)
library(rgeos)

##### load and preprocess loops data
loopsdata <- read.csv("../test/NDW_example/Rijkswegen_loops.csv")

names(loopsdata) <- c("site", "lat", "long", "roadname", "roadnumber")

loopsdata$roadname <- as.character(loopsdata$roadname)
loops.roadnumbers <- unique(loopsdata$roadnumber)

loops.roadnames <- loopsdata$roadname[match(loops.roadnumbers, loopsdata$roadnumber)]

loopsdata$roadname <- factor(loopsdata$roadname, levels=loops.roadnames)

loopsdata$roadname[is.na(loopsdata$roadname)] <- loops.roadnames[match(loopsdata$roadnumber[is.na(loopsdata$roadname)], loops.roadnumbers)]
	
loops <- SpatialPointsDataFrame(coords=loopsdata[,c("long", "lat")], data=loopsdata)
loops <- set_projection(loops, current.projection="longlat", projection="rd")

##### load and preprocess corop shape
corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

##### load and preprocess road shape
rw.roadnumbers <- unique(as.numeric(as.character(rw$WEGNUMMER)))

setdiff(rw.roadnumbers, loops.roadnumbers)
setdiff(loops.roadnumbers, rw.roadnumbers)

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(loops.roadnames[match(rw$roadnumber, loops.roadnumbers)], levels=loops.roadnames)

## simplify roads
rwb <- fit_polylines(rw, "roadname", na.rm=FALSE)

## split by corop
rwb_cr <- split_lines(rwb, corop)


########## assign loops to roads
d <- gDistance(rwb, loops, byid=TRUE)
d <- d[,1:(ncol(d)-1)]

closeID <- apply(d, MARGIN=1, FUN=function(i) which(i<25))

dimnames(d)

rwb_roadnames <- dimnames(d)[[2]]

loops$minID <- rwb_roadnames[apply(d, MARGIN=1, FUN=function(i) which.min(i))]
loops$closeID1 <- rwb_roadnames[sapply(closeID, function(i)if(length(i))i[[1]] else NA)]
loops$closeID2 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=2)i[[2]] else NA)]
loops$closeID3 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=3)i[[3]] else NA)]
loops$closeID4 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=4)i[[4]] else NA)]

loops$closeN <- as.integer(!is.na(loops$closeID1)) + as.integer(!is.na(loops$closeID2)) + 
	as.integer(!is.na(loops$closeID3)) + as.integer(!is.na(loops$closeID4))

loops$withinRange <- (loops$roadname == loops$closeID1 | loops$roadname == loops$closeID2 |
					loops$roadname == loops$closeID3 | loops$roadname == loops$closeID4)
loops$withinRange[is.na(loops$withinRange)] <- FALSE

loops$type <- ifelse(loops$withinRange & loops$closeN==1, "within 25m from correct road",
			  ifelse(loops$withinRange & loops$closeN>1, "multile roads within 25m",
			  ifelse(!loops$withinRange & loops$closeN>=1, "within 25m from other road",
			 "too far away from road")))



rwb <- get_shape("../test/NDW_example/rijksweg2013simpel.shp")
rwb@proj4string <- rw@proj4string

######## output: total road lengths
rwb$length <- SpatialLinesLengths(rwb, longlat=FALSE)/1000
data.frame(rijksweg=rwb$ID, lengte=round(total_lengths/1000, digits=2))

######## output: road lengths per corop
rwb_cr$length <- round(SpatialLinesLengths(rwb_cr, longlat=FALSE)/1000, digits=3)
length_cr_data <- rwb_cr@data[!is.na(rwb_cr$ID), c("ID", "CR_2013", "length")]

write.table(length_cr_data, file="../test/NDW_example/road_length_corop.csv", row.names=FALSE)


######## save shapes
require(rgdal)
writeOGR(rwb, "../test/NDW_example", "rijksweg2013simpel", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(rwb_cr, "../test/NDW_example", "rijksweg2013simpel_corop", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(loops, "../test/NDW_example", "loops", driver="ESRI Shapefile", overwrite_layer=TRUE)


