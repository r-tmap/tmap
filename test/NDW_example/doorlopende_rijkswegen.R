### load package
devtools::load_all()
library(sp)
library(rgeos)

###########################################################################
##### load and preprocess loops data
###########################################################################
loopsdata <- read.csv("../test/NDW_example/input/Rijkswegen_loops.csv", stringsAsFactors=FALSE)

names(loopsdata) <- c("site", "lat", "long", "roadname", "roadnumber")

loops.roadnumbers <- unique(loopsdata$roadnumber)
loops.roadnames <- loopsdata$roadname[match(loops.roadnumbers, loopsdata$roadnumber)]

loopsdata$roadname <- factor(loopsdata$roadname, levels=loops.roadnames)
loopsdata$roadname[is.na(loopsdata$roadname)] <- loops.roadnames[match(loopsdata$roadnumber[is.na(loopsdata$roadname)], loops.roadnumbers)]

loops <- SpatialPointsDataFrame(coords=loopsdata[,c("long", "lat")], data=loopsdata)
loops <- set_projection(loops, current.projection="longlat", projection="rd")

# save loops
save(loops, file="../test/NDW_example/throughput/loops.rda")


###########################################################################
##### load and preprocess corop shape
###########################################################################
corop <- get_shape("../test/NDW_example/input/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

nuts <- read.csv2("../test/NDW_example/COROP-NUTS.csv", stringsAsFactor=FALSE)
nuts$NUTS1 <- as.integer(substr(nuts$NUTS3, 3, 3))
nuts$NUTS2 <- as.integer(substr(nuts$NUTS3, 3, 4))

corop <- append_data(corop, data=nuts, key.shp="CR2013", key.data="COROP")

# save corop
save(corop, file="../test/NDW_example/throughput/corop.rda")


###########################################################################
##### load and preprocess road shape
###########################################################################

rw <- get_shape("../test/NDW_example/input/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

rw.roadnumbers <- unique(as.numeric(as.character(rw$WEGNUMMER)))

setdiff(rw.roadnumbers, loops.roadnumbers)
setdiff(loops.roadnumbers, rw.roadnumbers)

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(loops.roadnames[match(rw$roadnumber, loops.roadnumbers)], levels=loops.roadnames)

table(rw$roadnumber, rw$roadname, useNA="always")

rw <- rw[!is.na(rw$roadname),]



## simplify roads
rwb <- fit_polylines(rw, loops, id="roadname")

rwb$ID <- factor(as.character(rwb$ID), levels=levels(rw$roadname))

## split by corop
rwb_cr <- split_lines_poly(rwb, corop)




###### opslaan
save_shape(rwb, "../test/NDW_example/rw2013_doorgaand")
save_shape(rwb_cr, "../test/NDW_example/rw2013_doorgaand_cr")
save_shape(loops, "../test/NDW_example/loops2013")

