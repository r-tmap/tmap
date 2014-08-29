### load package
devtools::load_all()
library(sp)
library(rgeos)

### load preprocessed data
load("../applications/traffic_NLD//throughput/loops.rda")
load("../applications/traffic_NLD//throughput/rijkswegen.rda")
source("../applications/traffic_NLD//00_misc_functions.R")


###### TEMP SELECTION

road <- c("A2", "A58", "A79")

#rw <- rw[rw$roadname==road, ]
rwL <- rwL[rwL$roadname %in% road, ]
rwR <- rwR[rwR$roadname %in% road, ]
loops <- loops[loops$roadname %in% road,]


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
#roads <- read.csv2("../applications/traffic_NLD/throughput/roads.csv", stringsAsFactors=FALSE)


drwL3 <- set_directions(drwL2, main_direction = FALSE, directions = directions)
drwR3 <- set_directions(drwR2, main_direction = TRUE, directions = directions)


drwL4 <- create_meter_list(drwL3, dist=10, direction="L")
drwR4 <- create_meter_list(drwR3, dist=10, direction="R")


loopsL <- search_points(loops, drwL3)
loopsL <- lapply(loopsL, function(l) l[l$d<20, ]) ## todo: use loop meta data

loopsR <- search_points(loops, drwR3)
loopsR <- lapply(loopsR, function(l) l[l$d<20, ]) ## todo: use loop meta data

drwL4 <- write_point_info(loopsL, drwL4, "LUS")
drwR4 <- write_point_info(loopsR, drwR4, "LUS")



afrL <- search_POB(rwL[rwL$BAANSUBSRT=="AFR", ], drwL3) 
afrR <- search_POB(rwR[rwR$BAANSUBSRT=="AFR", ], drwR3) 



drwL4 <- write_point_info(afrL, drwL4, "AFRIT")
drwR4 <- write_point_info(afrR, drwR4, "AFRIT")



oprL <- search_POB(rwL[rwL$BAANSUBSRT=="OPR", ], drwL3) 
oprR <- search_POB(rwR[rwR$BAANSUBSRT=="OPR", ], drwR3) 

drwL4 <- write_point_info(oprL, drwL4, "OPRIT")
drwR4 <- write_point_info(oprR, drwR4, "OPRIT")

write_info(drwL4, path="../applications/traffic_NLD/output")
write_info(drwR4, path="../applications/traffic_NLD/output")

pdf("../applications/traffic_NLD/plots/rijkswegen_met_info.pdf", width = 10, height=10) 
plot_drw(drwL3, drwL4, scale=.1)
dev.off()

drw <- drwL3
info <- drwL4

plot_per_rw(drwL3, drwL4, path="../applications/traffic_NLD/plots")
plot_per_rw(drwR3, drwR4, path="../applications/traffic_NLD/plots")

plot_google(drwL3, drwL4, "A79")
plot_google(drwR3, drwR4, "A79")


info <- drwL4
rn <- "A79"

