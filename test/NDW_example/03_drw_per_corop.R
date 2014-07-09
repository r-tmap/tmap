### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../test/NDW_example/throughput/doorlopende_rijkswegen.rda")
load("../test/NDW_example/throughput/corop.rda")
source("../test/NDW_example/00_misc_functions.R")

## split by corop
drw_nuts <- split_lines_poly(drw, corop)

drwL_nuts <- split_lines_poly(drwL, corop)
drwR_nuts <- split_lines_poly(drwR, corop)


key <- paste(drw_nuts$ID, drw_nuts$NUTS3, sep="_")
keyL <- paste(drwL_nuts$ID, drwL_nuts$NUTS3, sep="_")
keyR <- paste(drwR_nuts$ID, drwR_nuts$NUTS3, sep="_")

# remove left overs
drwL_nuts <- drwL_nuts[keyL %in% key, ]
drwR_nuts <- drwR_nuts[keyR %in% key, ]

keyL <- keyL[keyL %in% key]
keyR <- keyR[keyR %in% key]


save(drw_nuts, drwL_nuts, drwR_nuts, file="../test/NDW_example/throughput/doorlopende_rijkswegen_nuts.rda")


### plots
library(RColorBrewer)
pal <- c(brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Dark2")[-7],
		 brewer.pal(12, "Set3")[-9])

pdf("../test/NDW_example/output/doorlopende_rijkswegen_nuts.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray70") +
	geo_borders("white") +
	#geo_shape(gm) +
	#geo_borders("white") +
	geo_shape(drw_nuts) +	
	geo_lines("ID", lwd=3, palette=pal, max.categories=46) +
	geo_theme(legend.show=FALSE, title="Doorlopende rijkswegen per NUTS3")
dev.off()







### output table length per doorlopende rijksweg
drw_nuts_len <- data.frame(roadname=as.character(drw_nuts$ID), nuts=drw_nuts$NUTS3, length=NA,lengthL=NA,lengthR=NA, stringsAsFactors=FALSE)


drw_nuts_len$length <- get_lengths(drw_nuts)
drw_nuts_len$lengthL[match(keyL, key)] <- get_lengths(drwL_nuts)
drw_nuts_len$lengthR[match(keyR, key)] <- get_lengths(drwR_nuts)

drw_nuts_len <- rbind(drw_nuts_len, data.frame(roadname="total", nuts="total", length=sum(drw_nuts_len$length, na.rm=TRUE), lengthL=sum(drw_nuts_len$lengthL, na.rm=TRUE), lengthR=sum(drw_nuts_len$lengthR, na.rm=TRUE)))

write.table(drw_nuts_len, file="../test/NDW_example/output/lengtes_drw_nuts3.csv", row.names=FALSE, sep=",")
