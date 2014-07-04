#rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
#rw <- set_projection(rw, current.projection="rd")

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

gm <- get_shape("../shapes/gm_2012.shp")
gm <- set_projection(gm, current.projection="rd")

rwb_cr <- get_shape("../test/NDW_example/rw2013_doorgaand_cr.shp")
loops <- get_shape("../test/NDW_example/loops2013_classified.shp")

library(RColorBrewer)

pal <- c(brewer.pal(9, "Pastel1")[-9], 
		 brewer.pal(8, "Pastel2")[-7],
		 brewer.pal(12, "Set3")[-9])

rwb_cr <- rwb_cr[!is.na(rwb_cr$ID), ]

is_N_weg <- substr(as.character(rwb_cr$ID), 1, 1)=="N"
require(sp)
rwb_cr$length <- SpatialLinesLengths(rwb_cr, longlat=FALSE)/1000
rwb_cr$label <- format(rwb_cr$length, trim=TRUE, digits=2)

rwb_code <- as.integer(substr(as.character(rwb_cr$ID), 2, 3))

rwb_cr$ID <- factor(as.character(rwb_cr$ID), levels=unique(as.character(rwb_cr$ID)))


library(RColorBrewer)

pal <- c(brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Dark2")[-7],
		 brewer.pal(12, "Set3")[-9])


nuts <- read.csv2("../test/NDW_example/COROP-NUTS.csv", stringsAsFactor=FALSE)
nuts$NUTS1 <- as.integer(substr(nuts$NUTS3, 3, 3))
nuts$NUTS2 <- as.integer(substr(nuts$NUTS3, 3, 4))


corop <- append_data(corop, data=nuts, key.shp="CR2013", key.data="COROP")


## rijkswegen per corop
pdf("../test/NDW_example/rw_corop.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray70") +
	geo_borders("white") +
	#geo_shape(gm) +
	#geo_borders("white") +
	geo_shape(rwb_cr) +	
	geo_lines("ID", lwd=3, palette=pal, max.categories=46) +
	geo_theme(legend.show=FALSE, title="Rijkswegen per corop")

dev.off()


## tabel rw_corop
rw_cr <- rwb_cr@data[!is.na(rwb_cr$ID), c("ID", "CR_2013")]
write.csv2(rw_cr, row.names=FALSE, file="../test/NDW_example/tabel_rw_corop.csv")


## experiment with output vis
str(rwb_cr@data)

nms <- paste(rep(paste("2014-05-", sprintf("%02d", 5:11), sep=""), each=5), 
			 rep(c("alle", "ochtend", "middag", "avond", "nacht"), times=7), sep="_")

for (n in nms) {
	rwb_cr[[n]] <- rnorm(136, mean=100, sd=4)
}


geo_shape(corop) +
	geo_fill("gray70") +
	geo_borders("white") +
	#geo_shape(gm) +
	#geo_borders("white") +
	geo_shape(rwb_cr) +	
	geo_lines("x", lwd=3, max.categories=46) +
	geo_theme(legend.show=TRUE, title="Test")


geo_shape(corop) +
	geo_fill("gray70") +
	geo_borders("white") +
	#geo_shape(gm) +
	#geo_borders("white") +
	geo_shape(rwb_cr) +	
	geo_lines("2014-05-05_alle", lwd=3, max.categories=46) +
	geo_theme(legend.show=TRUE, title="Test")


