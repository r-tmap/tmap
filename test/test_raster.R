library(raster)
library(sp)

data(World, land)


tmap_mode("plot")
tmap_mode("view")
repr <- FALSE
repr <- TRUE


## SpatialGridDF
tm_shape(land) +
	tm_raster("cover_cls") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()

tm_shape(land) +
	tm_raster("trees") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()


## RasterLayer
library(raster)

rl <- raster(land, layer=2)
tm_shape(rl) +
	tm_raster("cover_cls") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()

rl2 <- raster(land, layer=3)
tm_shape(rl2) +
	tm_raster("trees") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()


## Stack
rs <- stack(land)
tm_shape(rs) +
	tm_raster("cover_cls") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()

tm_shape(rs) +
	tm_raster("trees") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()


## Brick
rb <- brick(land)
tm_shape(rb) +
	tm_raster("cover_cls") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()

tm_shape(rb) +
	tm_raster("trees") +
	tm_shape(World, is.master = repr) +
	tm_borders() +
	tm_style_white()


## set_projection
land2 <- set_projection(land, projection="eck4")
qtm(land, raster="cover")
qtm(land2, raster="cover")

rlB <- set_projection(rl, projection="eck4")
qtm(rl, raster="cover_cls")
qtm(rlB, raster="cover_cls")

rl2B <- set_projection(rl2, projection="eck4")
qtm(rl2, raster="trees")
qtm(rl2B, raster="trees")


rs2 <- set_projection(rs, projection="eck4")
qtm(rs, raster="trees")
qtm(rs2, raster="trees")

rb2 <- set_projection(rb, projection="eck4")
qtm(rb, raster="trees")
qtm(rb2, raster="trees")



## test get_raster_data
system.time({rsdf <- raster::as.data.frame(rs)})
system.time({rsdf2 <- get_raster_data(rs)})

system.time({rbdf <- raster::as.data.frame(rb)})
system.time({rbdf2 <- get_raster_data(rb)})


## test RGB rasters
data(NLD_muni)
osm <- read_osm(NLD_muni, ext=1.2)

qtm(osm)


# download from http://www.terracolor.net/sample_imagery.html
# large raster (4323 x 4323)
x <- brick("../shape_files/TerraColor_SanFrancisco_US_15m.tif")

system.time({
	plotRGB(x)
})

system.time({
	print(qtm(x))
})

y <- aggregate_map(x, fact=32)

system.time({
	print(qtm(y))
})

tmap_mode("view")
qtm(y)

## download from http://www.ordnancesurvey.co.uk/docs/sample-data/25k-raster-sample-data.zip .
rosm <- raster("../shape_files/sx99.tif")
rosm <- set_projection(rosm, current.projection = "rd") # projection unknown
qtm(rosm)

# test raster smoothing
data(land)

land1 <- raster(land, layer="cover_cls")


x <- smooth_map(land1, cover.type="smooth")

