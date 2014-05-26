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


shpA2 <- fit_polylines(rwA2, "roadname")

pdf("../test/NDW_example/A2b.pdf", width=8, height=8)
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


#shp <- rw
#id <- "roadname"

rwb <- fit_polylines(rw, "roadname")

total_lengths <- SpatialLinesLengths(rwb, longlat=FALSE)
writeOGR(rwb, "../test/NDW_example", "rijksweg2013simpel", driver="ESRI Shapefile")


rwb <- get_shape("../test/NDW_example/rijksweg2013simpel.shp")
rwb@proj4string <- rw@proj4string

######### total road lengths
data.frame(rijksweg=rwb$ID, lengte=round(total_lengths/1000, digits=2))



library(RColorBrewer)
palDark <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"), brewer.pal(12, "Set3"))


########## roads simplified
pdf("../test/NDW_example/rw_simple2.pdf", width=16, height=16)
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

########## roads small multiples
pdf("../test/NDW_example/rw_mult.pdf", width=16, height=16)
geo_shape(corop) +
	geo_fill() +
	geo_borders() +
geo_shape(rwb) +
	geo_lines("ID", palette="red", by=TRUE)
dev.off()	


rwb_cr <- split_lines(rwb, corop)


########## roads per corop
pdf("../test/NDW_example/rw_per_corop.pdf", width=6, height=6)
geo_shape(corop) +
	geo_fill("CR_2013", palette="Pastel2") +
geo_shape(rwb_cr) +
	geo_lines("CR_2013", palette="Set2") +
geo_theme(legend.profile="hide", legend.max.categories=40)
dev.off()





########## assign loops to roads
system.time({
	x <- gDistance(rwb, loops, byid=TRUE)
})
closeID <- apply(x, MARGIN=1, FUN=function(i) which(i<25))

rwb_roadnames <- as.character(rwb$ID)

loops$minID <- rwb_roadnames[apply(x, MARGIN=1, FUN=function(i) which.min(i))]
loops$closeID1 <- rwb_roadnames[sapply(closeID, function(i)if(length(i))i[[1]] else NA)]
loops$closeID2 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=2)i[[2]] else NA)]
loops$closeID3 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=3)i[[3]] else NA)]
loops$closeID4 <- rwb_roadnames[sapply(closeID, function(i)if(length(i)>=4)i[[4]] else NA)]

loops$closeN <- as.integer(!is.na(loops$closeID1)) + as.integer(!is.na(loops$closeID2)) + 
	as.integer(!is.na(loops$closeID3)) + as.integer(!is.na(loops$closeID4))

loops$roadnames <- as.character(loops$ROADNUMBER)

loops$withinRange <- (loops$roadnames == loops$closeID1 | loops$roadnames == loops$closeID2 |
					loops$roadnames == loops$closeID3 | loops$roadnames == loops$closeID4)
loops$withinRange[is.na(loops$withinRange)] <- FALSE

loops$type <- ifelse(loops$withinRange & loops$closeN==1, "match",
			  ifelse(loops$withinRange & loops$closeN>1, "multile matches",
			  ifelse(!loops$withinRange & loops$closeN>=1, "wrong match",
			  "no match")))
	

pdf("../test/NDW_example/loops_classified.pdf", width=16, height=16)
geo_shape(corop) +
	geo_fill() +
	geo_shape(rwb) +
	geo_lines("ID", palette="Pastel2") +
	geo_shape(loops) +
	geo_bubbles(col="type", palette="Set1", size=.3)+
	geo_theme("Within 25m range?", legend.config="bubble.col", legend.max.categories=40)
dev.off()

