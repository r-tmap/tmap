### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD//throughput/loops.rda")
load("../applications/traffic_NLD//throughput/rijkswegen.rda")
source("../applications/traffic_NLD//00_misc_functions.R")


###### TEMP SELECTION

road <- c("A58", "A79")

#rw <- rw[rw$roadname==road, ]
rwL <- rwL[rwL$roadname %in% road, ]
rwR <- rwR[rwR$roadname %in% road, ]
loops <- loops[loops$roadname %in% road,]

plot(rwL)
plot(rwR, add=TRUE)

## CREATE CONTINUOUS POLYLINES

system.time({
	drwL <- fit_polylines(rwL[rwL$BAANSUBSRT=="HR", ], id="roadname")
})

system.time({
	drwR <- fit_polylines(rwR[rwR$BAANSUBSRT=="HR", ], id="roadname")
})


## SPLIT INTO EQUAL PARTS

dist <- 10
drwL2 <- split_lines_equal(drwL, dist=dist)
drwR2 <- split_lines_equal(drwR, dist=dist)

## REVERT IF NECASSARY

directions <- read.csv2("../applications/traffic_NLD/input/richtingen_rijkswegen.txt", stringsAsFactors=FALSE)
roads <- read.csv2("../applications/traffic_NLD/throughput/roads.csv", stringsAsFactors=FALSE)


lines <- drwL2
lines <- drwR2

drwL3 <- set_directions(drwL2, main_direction = FALSE, directions = directions, roads = roads)
drwR3 <- set_directions(drwR2, main_direction = TRUE, directions = directions, roads = roads)

str(coordinates(drwL3)[[1]])

drwLco <- mapply(function(co, id) {
	co <- lapply(co, function(co2) {
		co2 <- as.data.frame(co2)
		names(co2) <- c("x", "y")
		co2$roadname <- id
		co2$meter <- seq(0, by=dist, length.out=nrow(co2))
		co2$mark <- ""
		co2$mark[c(1, nrow(co2))] <- c("BEGIN", "EIND")
		co2
	})
	if (length(co)>1) {
		for (i in 2:length(co)) {
			co[[i]]$meter <- co[[i]]$meter + co[[i-1]]$meter[nrow(co[[i-1]])]
		}
	}
	co
}, coordinates(drwL3), drwL3$ID)


rwL_AFR <- rwL[rwL$BAANSUBSRT=="AFR", ]

rwL_AFR_co <- coordinates(rwL_AFR)[[1]]

rwL_AFR_co <- lapply(rwL_AFR_co, SpatialPoints)

afr_L <- map_points_to_line(shp.points=rwL_AFR_co[[3]], shp.lines=drwL2)



tm_shape(drwL) +
	tm_lines() +
	tm_shape(rwL[rwL$BAANSUBSRT=="AFR", ]) +
	tm_lines("blue")

save(drw, drwL, drwR, file="../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")

### diagnostics
pdf("../applications/traffic_NLD/output/doorlopende_rijkswegen.pdf", width=7, height=7)
tm_shape(corop) +
	tm_fill("gray65")+
#	tm_borders("white") +
tm_shape(drw) +
	tm_lines() +
tm_shape(rw) +
	tm_lines(lwd=.05, col="purple") +
tm_shape(loops) +
	tm_bubbles(size=.001, col="black") +
	tm_layout(scale=.2)
dev.off()

pdf("../applications/traffic_NLD/output/doorlopende_rijkswegenL.pdf", width=7, height=7)
tm_shape(corop) +
	tm_fill("gray65")+
	#	tm_borders("white") +
	tm_shape(drwL) +
	tm_lines() +
	tm_shape(rwL) +
	tm_lines(lwd=.05, col="purple") +
	tm_shape(loops) +
	tm_bubbles(size=.001, col="black") +
	tm_layout(scale=.2)
dev.off()

pdf("../applications/traffic_NLD/output/doorlopende_rijkswegenR.pdf", width=7, height=7)
tm_shape(corop) +
	tm_fill("gray65")+
	#	tm_borders("white") +
	tm_shape(drwR) +
	tm_lines() +
	tm_shape(rwR) +
	tm_lines(lwd=.05, col="purple") +
	tm_shape(loops) +
	tm_bubbles(size=.001, col="black") +
	tm_layout(scale=.2)
dev.off()


### output table length per doorlopende rijksweg
drw_len <- data.frame(roadname=levels(drw$ID), length=NA,lengthL=NA,lengthR=NA, stringsAsFactors=FALSE)



drw_len$length[as.integer(drw$ID)] <- get_lengths(drw)
drw_len$lengthL[as.integer(drwL$ID)] <- get_lengths(drwL)
drw_len$lengthR[as.integer(drwR$ID)] <- get_lengths(drwR)
drw_len <- rbind(drw_len, data.frame(roadname="total", length=sum(drw_len$length, na.rm=TRUE),
									 lengthL=sum(drw_len$lengthL, na.rm=TRUE), 
									 lengthR=sum(drw_len$lengthR, na.rm=TRUE)))

write.table(drw_len, file="../applications/traffic_NLD/output/lengtes_drw.csv", row.names=FALSE, sep=",")
