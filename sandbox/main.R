library(devtools)
check_man()
load_all()
library(stars)
library(sf)
library(data.table)
library(pryr)
library(profvis)


source("sandbox/test_data.R")



############ examples

## 1

tmel = tm_shape(land) +
	tm_raster("trees") +
tm_shape(World, name = "The World", is.main = TRUE) +
	tm_borders() +
tm_facets(by = "continent") +
tm_shape(metro) +
	tm_symbols(size = "pop2020")



############# process shapes

profvis::profvis({
	tmo = tmapObject(tmel)
})

mem_change({
	tmo = tmapObject(tmel)
})



tmg = tmo[[1]]
dt = tmg$tms$dt

tml = tmg$tmls[[1]]

dt2 = copy(dt)



lapply(tml$aes.mapping, tml$aes.mapping.fun)



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
