### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../test/NDW_example/throughput/rijkswegen.rda")
load("../test/NDW_example/throughput/doorlopende_rijkswegen.rda")
load("../test/NDW_example/throughput/corop.rda")
load("../test/NDW_example/throughput/doorlopende_rijkswegen_nuts.rda")
load("../test/NDW_example/throughput/loops.rda")
source("../test/NDW_example/00_misc_functions.R")


########## assign loops to roads
d <- gDistance(drw_nuts, loops, byid=TRUE)


drw_nuts$key <- factor(paste(drw_nuts$ID, drw_nuts$CR2013, sep="_"), levels=paste(drw_nuts$ID, drw_nuts$CR2013, sep="_"))

minsID <- apply(d, MARGIN=1, function(x)which.min(x)[1])
mins <- mapply(as.data.frame(t(d)), minsID, FUN=function(x, id)x[id], SIMPLIFY=TRUE)

minsKey <- drw_nuts$key[minsID]
minsKey[mins>5] <- NA



loops$drw <- minsKey



library(RColorBrewer)
pal <- c(brewer.pal(9, "Set1")[-9], 
		 brewer.pal(8, "Dark2")[-7],
		 brewer.pal(12, "Set3")[-9])

pdf("../test/NDW_example/output/loops_op_drw_nuts.pdf", width=7, height=7)
geo_shape(corop) +
	geo_fill("gray65")+
	geo_borders("white") +
	geo_shape(drw_nuts) +
	geo_lines(col="key", palette=pal, max.categories=150) +
	geo_shape(rw) +
	geo_lines(lwd=.05, col="black") +
	geo_shape(loops) +
	geo_bubbles(size=.001, col="drw", palette=pal, max.categories=150) +
	geo_theme(scale=.2)
dev.off()
