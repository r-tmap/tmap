rwb_cr <- get_shape(file="../test/NDW_example/rw2013_doorgaand_cr.shp")

library(sp)
######## output: road lengths per corop
rwb_cr$length <- round(SpatialLinesLengths(rwb_cr, longlat=FALSE)/1000, digits=3)
length_cr_data <- rwb_cr@data[!is.na(rwb_cr$ID), c("ID", "CR_2013", "length")]

write.table(length_cr_data, file="../test/NDW_example/road_length_corop.csv", row.names=FALSE)
