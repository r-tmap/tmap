

data(NLD_muni)
NLD_lim <- NLD_muni[NLD_muni$province=="Limburg",]

#### meuse


data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) <- TRUE

meuse.grid <- set_projection(meuse.grid, current.projection = "rd", overwrite.current.projection = TRUE)

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

land_stack <- stack(land_raster1, land_raster2)
land_brick <- brick(land_raster1, land_raster2)



tm_shape(land_stack) +
	tm_raster(c("cover", "trees"), max.categories = 20) +
tm_shape(World) +
	tm_borders()	

tm_shape(land_brick) +
	tm_raster("trees", palette="Greens") +
	tm_shape(World) +
	tm_borders()


### facets
tm_shape(land_stack) +
	tm_raster("cover", max.categories = 20) +
tm_facets(by="cover")
	

tm_shape(meuse.grid) +
	tm_raster("soil", max.categories = 20) +
	tm_facets(by="soil", free.coords = TRUE)

## test layers
tm_shape(meuse.grid) +
	tm_raster("soil", max.categories = 20) +
tm_shape(meuse.grid) +
	tm_raster("dist", alpha=.75)

