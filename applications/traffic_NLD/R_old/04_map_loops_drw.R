### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD/throughput/rijkswegen.rda")
load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD/throughput/corop.rda")
load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen_nuts.rda")
load("../applications/traffic_NLD/throughput/loops.rda")
source("../applications/traffic_NLD/00_misc_functions.R")


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

pdf("../applications/traffic_NLD/output/loops_op_drw_nuts.pdf", width=7, height=7)
tm_shape(corop) +
	tm_fill("gray65")+
	tm_borders("white") +
	tm_shape(drw_nuts) +
	tm_lines(col="key", palette=pal, max.categories=150) +
	tm_shape(rw) +
	tm_lines(lwd=.05, col="black") +
	tm_shape(loops) +
	tm_bubbles(size=.001, col="drw", palette=pal, max.categories=150) +
	tm_layout(scale=.2)
dev.off()
