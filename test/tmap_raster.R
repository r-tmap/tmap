

data(NLD_muni)
NLD_lim <- NLD_muni[NLD_muni$province=="Limburg",]



data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) <- TRUE

## SpatialPixelsDataFrame
shpPx <- meuse.grid
class(shpPx)

shpPx@bbox <- shpPx@bbox * 2
image(shpPx["dist"])


shpPx2 <- as(shpPx, "SpatialPixels")
class(shpPx2)

## SpatialGridDataFrame
shpGrid <- as(meuse.grid, "SpatialGridDataFrame")
class(shpGrid)


## raster
shpRst <- raster(meuse.grid, layer="dist")
class(shpRst)


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
	

library(rgdal)
library(raster)

#http://www.iscgm.org/gm/glcnmo.html


data(World)
x = readGDAL("../shapes/gm_lc_v1_simple2p.tif")
y = readGDAL("../shapes/gm_ve_v1_simple2p.tif")


x$band1cat <- factor(x$band1, levels=1:20, labels=
					 	c("Broadleaf Evergreen Forest", "Broadleaf Deciduous Forest",
					 	  "Needleleaf Evergreen Forest", "Needleleaf Deciduous Forest",
					 	  "Mixed Forest", "Tree Open",
					 	  "Shrub", "Herbaceous",
					 	  "Herbaceous with Sparse Tree/Shrub", "Sparse vegetation",
					 	  "Cropland", "Paddy field",
					 	  "Cropland / Other Vegetation Mosaic", "Mangrove",
					 	  "Wetland", "Bare area,consolidated(gravel,rock)",
					 	  "Bare area,unconsolidated (sand)", "Urban",
					 	  "Snow / Ice", "Water bodies")
)
y$band1[y$band1==254] <- NA


tm_shape(x) +
	tm_raster("band1cat", max.categories = 20) +
tm_shape(World) +
	tm_borders()	

tm_shape(y) +
	tm_raster("band1", palette="Greens") +
	tm_shape(World) +
	tm_borders()
