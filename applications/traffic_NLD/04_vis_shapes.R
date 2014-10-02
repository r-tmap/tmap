### load package
devtools::load_all()
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(treemap)

### load preprocessed data
load("../applications/traffic_NLD/throughput/viz_roads.rda")
load("../applications/traffic_NLD/throughput/corop.rda")
source("../applications/traffic_NLD/00_misc_functions.R")



######################################################################
########################## NUTS regions ##############################
######################################################################
nuts3 <- corop

cols <- treepalette(nuts3@data, index = c("NUTS1", "NUTS2", "NUTS3"), palette.HCL.options = list(hue_fraction=.8, hue_perm=TRUE, hue_rev=TRUE, hue_start=90))

nuts3 <- append_data(nuts3, cols[!is.na(cols$NUTS3),c("NUTS1", "NUTS2", "NUTS3", "HCL.color")], key.data = "NUTS3", key.shp = "NUTS3")
names(nuts3) <- gsub(".", "_", names(nuts3), fixed = TRUE)

nuts2 <- unionSpatialPolygons(nuts3, nuts3$NUTS2)
nuts2 <- append_data(nuts2, data=data.frame(NUTS2=get_IDs(nuts2)))
nuts2$HCL_color <- nuts3$HCL_color[match(nuts2$NUTS2, nuts3$NUTS2)]

nuts3$x <- rnorm(length(nuts3), mean = 100, sd=10)

nuts1 <- unionSpatialPolygons(nuts3, nuts3$NUTS1)
nuts1 <- append_data(nuts1, data=data.frame(NUTS1=get_IDs(nuts1)))
nuts1$HCL_color <- nuts3$HCL_color[match(nuts1$NUTS1, nuts3$NUTS1)]

tm_shape(nuts3) +
	tm_fill("HCL_color") + 
	tm_borders() +
	tm_shape(nuts2) +
	tm_borders(lwd=2)

nuts3 <- set_projection(nuts3, "longlat")
nuts2 <- set_projection(nuts2, "longlat")
nuts1 <- set_projection(nuts1, "longlat")

######################################################################
########################## roads #####################################
######################################################################

roads <- drw_viz
roadsL <- drw_vizL
roadsR <- drw_vizR

roads <- set_projection(roads, "longlat")
roadsL <- set_projection(drw_vizL, "longlat")
roadsR <- set_projection(drw_vizR, "longlat")


#### generate random data
set.seed(20140703)

m <- runif(length(roads), 0, 100)
mdiff <- rnorm(length(roads), 3, 15)
m[m+abs(mdiff) > 100] <- 100 - abs(mdiff[m+abs(mdiff) > 100])
m[m-abs(mdiff) < 0] <- abs(mdiff[m-abs(mdiff) < 0])

mR <- m + mdiff
mL <- m - mdiff

roads$x <- m
roadsL$x <- mL
roadsR$x <- mR


library(RColorBrewer)
brewer.pal(7, name = "YlOrRd")[3:7]

######################################################################
########################## places in NL ##############################
######################################################################

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

placesNL <- SpatialPointsDataFrame(placesNL[, c("X_GBA", "Y_GBA")], data = placesNL, match.ID = FALSE)
placesNL <- set_projection(placesNL, current.projection = "rd")

tm_shape(nuts2) +
	tm_fill("grey") +
	tm_borders() +
	tm_shape(placesNL) +
	tm_text("name", fontcolor = "black")

placesNL <- set_projection(placesNL, "longlat")

######################################################################
########################## Europe #### ###############################
######################################################################
#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries_lakes.zip", "../shapes/ne_10m_admin_0_countries_lakes.zip")
#unzip("../shapes/ne_10m_admin_0_countries_lakes.zip", exdir="../shapes")
world10 <- readOGR("../shapes", "ne_10m_admin_0_countries_lakes")



eu <- world10[world10$ADM0_A3 %in% c("NLD", "BEL", "FRA", "DEU", "LUX"),]
qtm(eu)


eu <- set_projection(eu, "longlat")

######################################################################
########################## write to geojson ##########################
######################################################################

shps <- c("nuts1", "nuts2", "nuts3", "roads", "roadsL", "roadsR", "placesNL", "eu")

lapply(shps, function(s){
	filename <- "../applications/traffic_NLD/prototype_vis/.geojson"
	filename2 <- paste0("../applications/traffic_NLD/prototype_vis/", s, ".js", sep="")
	unlink(filename)
	unlink(filename2)

	shp <- get(s, envir = .GlobalEnv)
	writeOGR(shp, filename, 'dataMap', driver='GeoJSON')
	
	input <- readLines(filename) 
	input[1] <- paste0("var ", s, " = ", input[1])
	writeLines(input, filename2) 
	invisible()
})

