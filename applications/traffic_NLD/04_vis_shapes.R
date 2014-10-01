### load package
devtools::load_all()
library(sp)
library(rgeos)
library(maptools)
library(rgdal)

### load preprocessed data
load("../applications/traffic_NLD/throughput/double_roads.rda")
load("../applications/traffic_NLD//throughput/doorlopende_rijkswegen.rda")
load("../applications/traffic_NLD/throughput/corop.rda")
source("../applications/traffic_NLD/00_misc_functions.R")

qtm(corop)

library(treemap)
str(corop@data)

cols <- treepalette(corop@data, index = c("NUTS1", "NUTS2", "NUTS3"), palette.HCL.options = list(hue_fraction=.8, hue_perm=TRUE, hue_rev=TRUE, hue_start=90))

corop <- append_data(corop, cols[!is.na(cols$NUTS3),c("NUTS1", "NUTS2", "NUTS3", "HCL.color")], key.data = "NUTS3", key.shp = "NUTS3")
names(corop) <- gsub(".", "_", names(corop), fixed = TRUE)

prov <- unionSpatialPolygons(corop, corop$NUTS2)
prov <- append_data(prov, data=data.frame(NUTS2=get_IDs(prov)))
prov$HCL_color <- corop$HCL_color[match(prov$NUTS2, corop$NUTS2)]

land <- unionSpatialPolygons(corop, corop$NUTS1)
land <- append_data(land, data=data.frame(NUTS1=get_IDs(land)))
land$HCL_color <- corop$HCL_color[match(land$NUTS1, corop$NUTS1)]



tm_shape(corop) +
	tm_fill("HCL_color") + 
	tm_borders() +
	tm_shape(prov) +
	tm_borders(lwd=2)

## create geojson objects
corop <- set_projection(corop, "longlat")
prov <- set_projection(prov, "longlat")
land <- set_projection(land, "longlat")
drw_nuts <- set_projection(drw_nuts, "longlat")

writeOGR(corop, '../applications/traffic_NLD/prototype_vis/nuts3.geojson','dataMap', driver='GeoJSON')
writeOGR(prov, '../applications/traffic_NLD/prototype_vis/nuts2.geojson','dataMap', driver='GeoJSON')
writeOGR(land, '../applications/traffic_NLD/prototype_vis/nuts1.geojson','dataMap', driver='GeoJSON')
writeOGR(drw_nuts, '../applications/traffic_NLD/prototype_vis/drw_nuts.geojson','dataMap', driver='GeoJSON')

## rewrite as js files
input<-readLines("../applications/traffic_NLD/prototype_vis/nuts3.geojson") 
input[1] <- paste("var nuts3 =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/nuts3.js") 

input<-readLines("../applications/traffic_NLD/prototype_vis/nuts2.geojson") 
input[1] <- paste("var nuts2 =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/nuts2.js") 

input<-readLines("../applications/traffic_NLD/prototype_vis/nuts1.geojson") 
input[1] <- paste("var nuts1 =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/nuts1.js") 

input<-readLines("../applications/traffic_NLD/prototype_vis/drw_nuts.geojson") 
input[1] <- paste("var drw =", input[1])
writeLines(input, "../applications/traffic_NLD/prototype_vis/drw_nuts.js") 

rm(input); gc()


########### places
shp <- read_shape("../shapes/bevolkingskern_2011.shp")
kernNL <- shp[shp$BEV11TOT>=5000,]

# tm_shape(NLD_prov) +
# 	tm_fill("grey") +
# 	tm_borders() +
# 	tm_shape(kernNL) +
# 	tm_fill("blue") +
# 	tm_text("KERN_NAAM", fontcolor = "black", cex="BEV11TOT")

placesNL <- kernNL@data


placesNL$name <- as.character(placesNL$KERN_NAAM)
placesNL$name <- sub("Groot - ", "", placesNL$name, fixed = TRUE)
placesNL$name <- sub(" \\((.*?)\\)", "", placesNL$name)
placesNL$name <- sub("/.*", "", placesNL$name)
placesNL$name <- sub("-Kern", "", placesNL$name, fixed = TRUE)
placesNL$size <- as.integer(substr(as.character(placesNL$BKGR_CODE), 5,6))

placesNL <- placesNL[,c("name", "X_GBA", "Y_GBA", "size")]


library(sp)
placesNL <- SpatialPointsDataFrame(placesNL[, c("X_GBA", "Y_GBA")], data = placesNL, match.ID = FALSE)
placesNL <- set_projection(placesNL, current.projection = "rd")

tm_shape(NLD_prov) +
	tm_fill("grey") +
	tm_borders() +
	tm_shape(placesNL) +
	tm_text("name", fontcolor = "black")

placesNL <- set_projection(placesNL, "longlat")

library(rgdal)
writeOGR(placesNL, '../applications/traffic_NLD/prototype_vis/placesNL.geojson','dataMap', driver='GeoJSON')
