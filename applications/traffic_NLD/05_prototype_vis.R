### load package
devtools::load_all()
library(sp)
library(rgeos)
library(maptools)
library(rgdal)

### load preprocessed data
load("../applications/traffic_NLD/throughput/rijkswegen.rda")
load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD/throughput/corop.rda")
load("../applications/traffic_NLD/throughput/doorlopende_rijkswegen_nuts.rda")
load("../applications/traffic_NLD/throughput/loops.rda")
source("../applications/traffic_NLD/00_misc_functions.R")

qtm(corop)

library(treemap)
str(corop@data)

cols <- treepalette(corop@data, index = c("NUTS1", "NUTS2", "NUTS3"), palette.HCL.options = list(hue_fraction=.8, hue_perm=TRUE, hue_rev=TRUE, hue_start=90))

corop <- append_data(corop, cols[!is.na(cols$NUTS3),c("NUTS3", "HCL.color")], key.data = "NUTS3", key.shp = "NUTS3")
names(corop) <- gsub(".", "_", names(corop), fixed = TRUE)

prov <- unionSpatialPolygons(corop, corop$NUTS2)
prov <- append_data(prov, data=data.frame(ID=get_IDs(prov)))

tm_shape(corop) +
	tm_fill("HCL_color") + 
	tm_borders() +
tm_shape(prov) +
	tm_borders(lwd=2)

## create geojson objects
corop <- set_projection(corop, "longlat")
prov <- set_projection(prov, "longlat")
drw_nuts <- set_projection(drw_nuts, "longlat")

writeOGR(corop, '../applications/traffic_NLD/prototype_vis/corop.geojson','dataMap', driver='GeoJSON')
writeOGR(prov, '../applications/traffic_NLD/prototype_vis/prov.geojson','dataMap', driver='GeoJSON')
writeOGR(drw_nuts, '../applications/traffic_NLD/prototype_vis/drw_nuts.geojson','dataMap', driver='GeoJSON')

## rewrite as js files
input<-readLines("../applications/traffic_NLD/prototype_vis/corop.geojson") 
input[1] <- paste("var corop =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/corop.js") 

input<-readLines("../applications/traffic_NLD/prototype_vis/prov.geojson") 
input[1] <- paste("var prov =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/prov.js") 

input<-readLines("../applications/traffic_NLD/prototype_vis/drw_nuts.geojson") 
input[1] <- paste("var drw =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/drw_nuts.js") 

rm(input); gc()


