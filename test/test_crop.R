data(land)

data(World)
data(NLD_muni)

NLD_muni <- set_projection(NLD_muni, projection = "longlat")

bb_NLD <- NLD_muni@bbox


landNL <- crop_shape(land, bb_NLD)

class(landNL)
tm_shape(landNL) +
	tm_raster("cover_cls") +
	tm_shape(NLD_muni) +
	tm_borders()

b <- brick(land)
bNL <- crop_shape(b, bb_NLD)
tm_shape(bNL) +
	tm_raster("cover_cls") +
	tm_shape(NLD_muni) +
	tm_borders()

land@proj4string <- NLD_muni@proj4string

tm_shape(NLD_muni) +
	tm_borders() +
tm_shape(land) +
	tm_raster("cover_cls") +
tm_shape(NLD_muni) +
	tm_borders()
	
shp <- land
data <- shp@data

shp@data <- data.frame(ID=1:nrow(data))
shp <- as(shp, "RasterLayer")

x <- get_Raster_data(shp)

shp2 <- crop_shape(shp, bbox = bb_NLD)
y <- get_Raster_data(shp2)


## test cropping shape with raster package
library(raster)

data(World)
data(NLD_muni)

NLD_muni <- set_projection(NLD_muni, projection = "longlat")
World <- set_projection(World, projection = "longlat")
bb_NLD <- NLD_muni@bbox

system.time({
Wnl <- crop(World, bb_NLD)
})


system.time({
	Wnl <- crop_shape(World, bb_NLD)
})


qtm(Wnl)









data(NLD_muni)
data(Europe)
data(World)
data(land)
data(metro)


tm_shape(World) +
	tm_fill() +
tm_shape(land) +
	tm_raster("trees") +
tm_shape(World) +
	tm_borders() +
tm_shape(Europe) +
	tm_borders(lwd = 3)

tm_shape(World) +
	tm_fill() +
	tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World) +
	tm_borders() +
	tm_shape(Europe) +
	tm_borders(lwd = 3)


tm_shape(land) + tm_raster("trees") + tm_shape(World) + tm_borders()
tm_shape(land) + tm_raster("blue") + tm_shape(World) + tm_borders()

land$color <- "#FFEE00"

## test speed
tm_fast <- tm_shape(land) + tm_raster("cover_cls") + tm_shape(World) + tm_borders()
tm_slow <- tm_shape(land) + tm_raster("blue") + tm_shape(World) + tm_borders()
tm_med <- tm_shape(land) + tm_raster("color") + tm_shape(World) + tm_borders()


Rprof(tmp <- tempfile())
#print(tm_slow)
print(tm_med)
#print(tm_fast)
Rprof()
summaryRprof(tmp)
unlink(tmp)

### "blue" conversion via rgb