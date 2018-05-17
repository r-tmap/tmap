library(tmaptools)

library(devtools)
install_github("edzer/sfr")

library(sf)
library(dplyr)

nc <- st_read(system.file("shape/nc.shp", package="sf"))


tm_shape(nc) +
	tm_polygons("BIR74")

ttm()
tm_shape(nc) +
	tm_text("FIPS")

tm_shape(nc) +
	tm_polygons(names(nc)[1:4]) +
	tm_layout(panel.labels = names(nc)[1:4])


library(mapview)

ncsp <- as(nc, "Spatial")
ms <- lapply(1:4, function(i) {
	mapview(ncsp, zcol=names(nc)[i])
})

do.call(sync, ms)

sync(ms[[2]], ms[[1]])

tm_shape(nc) +
	tm_polygons("BIR74")






ttm()

tm_shape(nc) +
	tm_polygons(names(nc)[1:4]) +
	tm_layout(panel.labels = names(nc)[1:4]) +
	tm_facets(free.coords=FALSE)

devtools::session_info()


nc$CNTY_ID2 <- cut(nc$CNTY_ID, 4)
tm_shape(nc) +
	tm_polygons("BIR74") +
tm_facets(by="CNTY_ID2", free.coords = TRUE)




## Not run: 
library(sp)
library(raster)

data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

## view different aspects of same data set
m1 <- mapview(meuse, zcol = "soil", burst = TRUE)
m2 <- mapview(meuse, zcol = "lead")
m3 <- mapview(meuse, zcol = "landuse", map.types = "Esri.WorldImagery")
m4 <- mapview(meuse, zcol = "dist.m")

latticeView(m1, m2, m3, m4) # 4 panels
sync(m1, m2, m3, m4) # 4 panels synchronised






