data(land)

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
	tm_raster("cover_cls")

shp <- land
data <- shp@data

shp@data <- data.frame(ID=1:nrow(data))
shp <- as(shp, "RasterLayer")

x <- get_Raster_data(shp)

shp2 <- crop_shape(shp, bbox = bb_NLD)
y <- get_Raster_data(shp2)

