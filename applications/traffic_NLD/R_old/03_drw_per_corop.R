### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD/throughput/corop.rda")
source("../applications/traffic_NLD/00_misc_functions.R")

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




### plots
library(RColorBrewer)
pal <- c(brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Dark2")[-7],
		 brewer.pal(12, "Set3")[-9],
		 brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Dark2")[-7],
		 brewer.pal(12, "Set3")[-9])
pal[c(10, 27)] <- pal[c(43, 16)]

pdf("../applications/traffic_NLD/output/doorlopende_rijkswegen_nuts.pdf", width=7, height=7)
tm_shape(corop) +
	tm_fill("gray70") +
	tm_borders("white") +
	#tm_shape(gm) +
	#tm_borders("white") +
	tm_shape(drw_nuts) +	
	tm_lines("ID", lwd=3, palette=pal, max.categories=46) +
	tm_layout(legend.show=FALSE, title="Doorlopende rijkswegen per NUTS3")
dev.off()



## for publication/presentation
png("../applications/traffic_NLD/plots/highways.png", width=1400, height=1600, res=300)
tm_shape(corop) +
	tm_fill("gray70") +
	tm_borders("white") +
	#tm_shape(gm) +
	#tm_borders("white") +
	tm_shape(drw_nuts) +	
	tm_lines("ID", lwd=3, palette=pal, max.categories=46) +
	tm_layout(legend.show=FALSE, title="Highways per NUTS3")
dev.off()





### output table length per doorlopende rijksweg
drw_nuts_len <- data.frame(roadname=as.character(drw_nuts$ID), nuts=drw_nuts$NUTS3, length=NA,lengthL=NA,lengthR=NA, stringsAsFactors=FALSE)


drw_nuts_len$length <- get_lengths(drw_nuts)
drw_nuts_len$lengthL[match(keyL, key)] <- get_lengths(drwL_nuts)
drw_nuts_len$lengthR[match(keyR, key)] <- get_lengths(drwR_nuts)

drw_nuts_len <- rbind(drw_nuts_len, data.frame(roadname="total", nuts="total", length=sum(drw_nuts_len$length, na.rm=TRUE), lengthL=sum(drw_nuts_len$lengthL, na.rm=TRUE), lengthR=sum(drw_nuts_len$lengthR, na.rm=TRUE)))

write.table(drw_nuts_len, file="../applications/traffic_NLD/output/lengtes_drw_nuts3.csv", row.names=FALSE, sep=",")

save(drw_nuts, drwL_nuts, drwR_nuts, file="../applications/traffic_NLD/throughput/doorlopende_rijkswegen_nuts.rda")

#load(file="../applications/traffic_NLD/throughput/doorlopende_rijkswegen_nuts.rda")
