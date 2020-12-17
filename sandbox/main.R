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
tm_shape(metro) +
	tm_symbols(size = "pop2020")


############# process shapes

profvis::profvis({
	tmo = tmapObject(tmel)
})


tml = tmel[[2]]

tmapLayer = function(tml) {
	
}








tmapShps = lapply(tmls, function(ti) do.call(.tmapShape, ti))




s = assign_values(tmls[[1]]$shp, dt = tmls[[1]]$dt, column = "cover")


s = assign_values(tmls[[4]]$shp, dt = tmls[[4]]$dt, column = "cover")



x = do.call(tmapShape, tmls[[1]])




x = tmls[[1]]

shp = x[[1]]
is.main = x[[2]]
crs = x[[3]]
bbox = x[[4]]
unit = x[[5]]
shp_name = x[[6]]


x$a=1

do.call(.tmapShape, tmls[[1]])


as.data.table(shp)
shp = sf::st_as_sf(shp)

has_raster(shp)
r = attr(shp, "raster")

attr(x, "raster")

get_raster(x)




get_empty_raster = function(shp, shp_name) {
	
	
	
	shp2 = shp
	
	
	
	shp2[[1]]
	
	
}




m = matrix(1:12, nrow = 3)
data = data.frame(x = c(35, 27, 44), TMAP__ = c(1, 3, 8))

m2 = matrix(NA, nrow = nrow(m), ncol = ncol(m))

m[data$TMAP__] = data$x



w = st_as_stars(World)




