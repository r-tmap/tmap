

data(NLD_muni)
NLD_lim <- NLD_muni[NLD_muni$province=="Limburg",]

#### meuse
library(raster)


data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) <- TRUE

meuse.grid <- set_projection(meuse.grid, current.projection = "rd", overwrite.current.projection = TRUE)
data(NLD_muni)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_grid()

tm_shape(NLD_muni, xlim=c(170000, 190000), ylim=c(320000, 340000), relative=FALSE) +
	tm_borders() +
	tm_text("name") +
tm_shape(meuse.grid) +
	tm_raster("dist", contrast=-4)
	
## SpatialPixelsDataFrame
shpPx <- meuse.grid
class(shpPx)

## SpatialGridDataFrame
shpGrid <- as(meuse.grid, "SpatialGridDataFrame")
class(shpGrid)


## raster
library(raster)
shpRst <- raster(meuse.grid, layer="dist")
class(shpRst)


data(rivers)
tm_shape(NLD_muni, ylim = c(0, .1), xlim=c(.6, .8)) +
	tm_fill("population", palette = "Blues") +
	tm_borders() +
tm_shape(shpPx) +
	tm_raster("dist") + 
tm_shape(rivers) +
	tm_lines()

tm_shape(NLD_lim) +
	tm_fill() +
	tm_borders() +
tm_shape(shpPx) +
	tm_raster("dist")


tm_shape(shpPx) +
	tm_raster("dist")

tm_shape(shpGrid) +
	tm_raster("dist")


#### rworldmap
library(rworldmap)
data(gridExData,envir=environment(),package="rworldmap")
str(gridExData@data)

WorldLL <- set_projection(World, "longlat")
gridExData <- set_projection(gridExData, current.projection = "longlat", overwrite.current.projection = TRUE)


data(World)
tm_shape(WorldLL) +
	tm_borders() +
tm_shape(gridExData) +
	tm_raster("pa2000.asc")


tm_shape(gridExData) +
	tm_raster("pa2000.asc") +
tm_shape(WorldLL) +
	tm_borders()
	

#### land

data(land)
data(World)

land_px <- as(land, "SpatialPixelsDataFrame")

land_b <- as(land, "RasterBrick")
land_s <- as(land, "RasterStack")

land_raster1 <- raster(land, layer=1)
land_raster2 <- raster(land, layer=2)
land_raster3 <- raster(land, layer=3)

land_stack <- stack(land_raster1, land_raster2, land_raster3)
land_brick <- brick(land_raster1, land_raster2, land_raster3)

tm_shape(land_b) + tm_raster(c("trees", "cover_cls"))
tm_shape(land_brick) + tm_raster(c("trees", "cover_cls"))
tm_shape(land_s) + tm_raster(c("trees", "cover_cls")) # no attribute data
tm_shape(land_stack) + tm_raster(c("trees", "cover_cls"))


