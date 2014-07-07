devtools::load_all()
library(sp)
library(rgeos)

##### load and preprocess loops data
loopsdata <- read.csv("../test/NDW_example/Rijkswegen_loops.csv")

names(loopsdata) <- c("site", "lat", "long", "roadname", "roadnumber")

loopsdata$roadname <- as.character(loopsdata$roadname)
loops.roadnumbers <- unique(loopsdata$roadnumber)

loops.roadnames <- loopsdata$roadname[match(loops.roadnumbers, loopsdata$roadnumber)]

loopsdata$roadname <- factor(loopsdata$roadname, levels=loops.roadnames)

loopsdata$roadname[is.na(loopsdata$roadname)] <- loops.roadnames[match(loopsdata$roadnumber[is.na(loopsdata$roadname)], loops.roadnumbers)]
	
loops <- SpatialPointsDataFrame(coords=loopsdata[,c("long", "lat")], data=loopsdata)
loops <- set_projection(loops, current.projection="longlat", projection="rd")

##### load and preprocess corop shape
corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

rw <- get_shape("../test/NDW_example/rijksweg2013.shp")
rw <- set_projection(rw, current.projection="rd")

##### load and preprocess road shape
rw.roadnumbers <- unique(as.numeric(as.character(rw$WEGNUMMER)))

setdiff(rw.roadnumbers, loops.roadnumbers)
setdiff(loops.roadnumbers, rw.roadnumbers)

rw$roadnumber <- as.numeric(as.character(rw$WEGNUMMER))
rw$roadname <- factor(loops.roadnames[match(rw$roadnumber, loops.roadnumbers)], levels=loops.roadnames)

## simplify roads
rwb <- fit_polylines(rw, "roadname", na.rm=FALSE)

rwb$ID <- factor(as.character(rwb$ID), levels=levels(rw$roadname))

## split by corop
rwb_cr <- split_lines_poly(rwb, corop)




###### opslaan
save_shape(rwb, "../test/NDW_example/rw2013_doorgaand")
save_shape(rwb_cr, "../test/NDW_example/rw2013_doorgaand_cr")
save_shape(loops, "../test/NDW_example/loops2013")


