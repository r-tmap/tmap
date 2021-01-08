library(devtools)
check_man()
load_all()
library(stars)
library(sf)
library(data.table)
library(pryr)
library(profvis)


source("sandbox/test_data.R")
World$gdp_est_mln = World$gdp_cap_est * World$pop_est / 1e6


############ examples

## 1

# tmel = tm_shape(land) +
# 	tm_raster("trees") +
# tm_shape(World, name = "The World", is.main = TRUE) +
# 	tm_borders() +
# tm_facets(by = "continent") +
# tm_shape(metro) +
# 	tm_symbols(size = "pop2020")




tmel = tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_cartogram(fill = "economy", size = c("pop_est", "gdp_est_mln")) +
	tm_symbols(color = c("blue", "red"), size = "pop_est") +
	tm_facets(by = "continent")
	


############# pipeline

# restructure to tmapObject
tmo = tmapObject(tmel)

# prepare data for transformation and mapping

updateData(tmo)

tmo[[1]]$tms$dt
tmo[[2]]$tms$dt





#################################################################
#### other end of the bridge:
#################################################################

World = st_transform(World, crs = 4326)

x = st_geometry(World)



fill = pals::brewer.blues(7)[as.integer(World$economy)]
color = "black"

bbx = st_bbox(World[23,])

# grid
tmapGridInit(bbx)
tmapGridPolygons(x, fill = fill, color = color)
tmapGridRun()

# leaflet
tmapLeafletInit(bbx)
tmapLeafletPolygons(x, fill = fill, color = color)
tmapLeafletRun()
