### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
#load("../applications/traffic_NLD//throughput/rijkswegen.rda")
load("../applications/traffic_NLD//throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD//throughput/corop.rda")
source("../applications/traffic_NLD//00_misc_functions.R")



############ PROCESS DOUBLE ROADS ##############################


drw_viz <- split_lines_poly(drwR, corop)

drw_list <- double_line(drw_viz, width = 500)

drw_vizL <- drw_list[[1]]
drw_vizR <- drw_list[[2]]
# 
# drw_vizL$NUTS_ID <- factor(paste(drw_vizL$NUTS3, drw_vizL$ID, sep="_"))
# drw_vizR$NUTS_ID <- factor(paste(drw_vizR$NUTS3, drw_vizR$ID, sep="_"))
# 
# drw_vizL <- fit_polylines(drw_vizL, id="NUTS_ID")
# drw_vizR <- fit_polylines(drw_vizR, id="NUTS_ID")
# 
# drw_nutsL2 <- split_lines_equal(drw_nutsL, dist=100)
# drw_nutsR2 <- split_lines_equal(drw_nutsR, dist=100)
# 
# nutsIDsL <- strsplit(as.character(drw_nutsL$ID), split = "_", fixed=TRUE)
# drw_nutsL2$NUTS3 <- sapply(nutsIDsL, function(x)x[1]) 
# drw_nutsL2$road <- sapply(nutsIDsL, function(x)x[2]) 
# 
# nutsIDsR <- strsplit(as.character(drw_nutsR$ID), split = "_", fixed=TRUE)
# drw_nutsR2$NUTS3 <- sapply(nutsIDsR, function(x)x[1]) 
# drw_nutsR2$road <- sapply(nutsIDsR, function(x)x[2]) 


pdf("../applications/traffic_NLD/plots/doubled_road.pdf", width=10, height=10)
tm_shape(corop) +
	tm_fill(col="CR_2013", max.categories = 50) +
	tm_shape(drw_vizL) +
	tm_lines(col="red") +
	tm_shape(drw_vizR) +
	tm_lines(col="blue") +
	tm_layout(scale=2)
dev.off()

save(drw_viz, drw_vizL, drw_vizR, file="../applications/traffic_NLD/throughput/viz_roads.rda")

