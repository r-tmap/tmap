### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD//throughput/loops.rda")
load("../applications/traffic_NLD//throughput/rijkswegen.rda")
load("../applications/traffic_NLD//throughput/corop.rda")
source("../applications/traffic_NLD//00_misc_functions.R")


directions <- read.csv2("../applications/traffic_NLD/input/richtingen_rijkswegen.txt", stringsAsFactors=FALSE)


###### TEMP SELECTION

afrL <- rwL[rwL$BAANSUBSRT=="AFR", ]
afrR <- rwR[rwR$BAANSUBSRT=="AFR", ]

oprL <- rwL[rwL$BAANSUBSRT=="OPR", ]
oprR <- rwR[rwR$BAANSUBSRT=="OPR", ]


rwL <- rwL[rwL$BAANSUBSRT=="HR", ]
rwR <- rwR[rwR$BAANSUBSRT=="HR", ]

# road <- c("A2", "A58", "A79")
# 
# rwL <- rwL[rwL$roadname %in% road, ]
# rwR <- rwR[rwR$roadname %in% road, ]
# loops <- loops[loops$roadname %in% road,]



### CREATE CONTINUOUS POLYLINES

# phase 1: create 100m points, combine road segments, set direction
system.time({
	rwL <- split_lines_equal(rwL, dist=100, include.last = TRUE)
	rwR <- split_lines_equal(rwR, dist=100, include.last = TRUE)

	drwL <- fit_polylines(rwL, id="roadname", 
						  min.dist=0, max.opt.dist=10, sep.dist=150) # was: 10, 250, 5000
	drwR <- fit_polylines(rwR, id="roadname",
							  min.dist=0, max.opt.dist=10, sep.dist=150)

	drwL <- set_directions(drwL, main_direction = FALSE, directions = directions)
	drwR <- set_directions(drwR, main_direction = TRUE, directions = directions)
})

# visual inspection
pdf("../applications/traffic_NLD/plots/rw.pdf")
tm_shape(corop) +
	tm_fill() +
	tm_shape(rwR) +
	tm_lines(col="red") +
	tm_shape(rwL) +
	tm_lines(col="blue") +
	tm_layout(scale=.15)
dev.off()

pdf("../applications/traffic_NLD/plots/drw.pdf")
tm_shape(corop) +
	tm_fill() +
tm_shape(drwR) +
	tm_lines(col="red") +
tm_shape(drwL) +
	tm_lines(col="blue") +
	tm_text("ID") +
tm_layout(scale=.15)
dev.off()



# plot per road * direction

plot_roads(drwL, appendix="L", map="../applications/traffic_NLD/plots/drw_per_road/")
plot_roads(drwR, appendix="R", map="../applications/traffic_NLD/plots/drw_per_road/")


# number of polylines per road*dir
nPoly <- data.frame(road=drwL$ID, 
					L=sapply(drwL@lines, function(x)length(x@Lines)), 
					R=sapply(drwR@lines, function(x)length(x@Lines)))


# phase 2: combine uncomplete roads (N33, N36, N48, N61, N99: use loops coordinates)
roads_combine <- list(A9=-1, A15=3:18, A31=4:6, N33=0, N36=0, N48=0, A50=2:3, N57=0, A59=list(L=4:14), N61=0, N99=0)

res <- combine_road_segments(drwL, drwR, roads_combine)

drwL <- res[[1]]
drwR <- res[[2]]

roads_rm_junk <- c("A4", "A15")
res <- remove_junk_segments(drwL, drwR, roads_rm_junk)

drwL <- res[[1]]
drwR <- res[[2]]



save(drwL, drwR, file="../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")

load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")




############ FOR ROAD SEGMENTS ##############################



## SPLIT INTO EQUAL PARTS

dist <- 10
drwL2 <- split_lines_equal(drwL, dist=dist)
drwR2 <- split_lines_equal(drwR, dist=dist)


listL <- create_meter_list(drwL2, dist=10, direction="L")
listR <- create_meter_list(drwR2, dist=10, direction="R")


loopsL <- search_points(loops[loops$direction=="L" | is.na(loops$direction), ], drwL2)
loopsL[-45] <- lapply(loopsL[-45], function(l) l[l$d<20, ]) ## todo: use loop meta data

loopsR <- search_points(loops[loops$direction=="R" | is.na(loops$direction), ], drwR2)
loopsR[-45] <- lapply(loopsR[-45], function(l) l[l$d<20, ]) ## todo: use loop meta data

listL <- write_point_info(loopsL, listL, "SENSOR")
listR <- write_point_info(loopsR, listR, "SENSOR")



listAfrL <- search_POB(afrL, drwL2) 
listAfrR <- search_POB(afrR, drwR2) 



listL <- write_point_info(listAfrL, listL, "EXIT")
listR <- write_point_info(listAfrR, listR, "EXIT")


listOprL <- search_POB(oprL, drwL2) 
listOprR <- search_POB(oprR, drwR2) 

listL <- write_point_info(listOprL, listL, "ENTER")
listR <- write_point_info(listOprR, listR, "ENTER")

write_info(listL, path="../applications/traffic_NLD/output")
write_info(listR, path="../applications/traffic_NLD/output")

save(listL, listR, file="../applications/traffic_NLD/output/road_list_info.rdata")

