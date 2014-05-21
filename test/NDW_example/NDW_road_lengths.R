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


# geo_shape(corop) +
# 	geo_fill() +
# geo_shape(rw) +
# 	geo_lines(col="roadname", by=TRUE)


## case study: A2
# volgens Google Maps 216 km van A'dam tot grens met Belgie

rwA2 <- rw[rw$roadname =="A2",]
loopsA2 <- loops[which(loops$ROADNUMBER=="A2"), ]

source("../test/NDW_example/NDW_functions.R")

shpA2 <- simplifyRoad(rwA2, "roadname")

pdf("../test/NDW_example/A2.pdf", width=8, height=8)
geo_shape(corop) +
	geo_borders() +
	geo_fill() +
geo_shape(rwA2) +
	geo_lines(col="red", lwd=.01) +
geo_shape(loopsA2) +
	geo_bubbles(col="steelblue", size=.01) +
geo_shape(shpA2) + 
	geo_lines(col="blue", lwd=.01) +
geo_theme("A2")
dev.off()


rwb <- simplifyRoad(rw, "roadname")

total_lengths <- SpatialLinesLengths(rwb, longlat=FALSE)
writeOGR(rwb, "../test/NDW_example", "rijksweg2013simpel", driver="ESRI Shapefile")

data.frame(rijksweg=rwb$ID, lengte=round(total_lengths/1000, digits=2))

library(RColorBrewer)
palDark <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"), brewer.pal(12, "Set3"))

pdf("../test/NDW_example/rw_simple.pdf", width=16, height=16)
geo_shape(corop) +
	#geo_borders() +
	geo_fill() +
	geo_shape(loops) +
	geo_bubbles(col="black", size=.01) +
	geo_shape(rwb) + 
	geo_lines(col="ID", lwd=.5, palette=palDark) +
	geo_text("ID", cex=1) +
	geo_shape(rw) +
	geo_lines(col="black", lwd=.02) +
	geo_theme("Origine rijkswegen (dunne zwarte lijntjes) +\nVerkeerlslussen (zwarte bolletjes) +\nVereenvoudigde rijkswegen (gekleurde lijnen)")
dev.off()

