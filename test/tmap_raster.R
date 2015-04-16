library(sp)
library(raster)

data(meuse.grid)
m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid)

str(m)

coordinates(meuse.grid) = c("x", "y")
gridded(meuse.grid) <- TRUE
x = as(meuse.grid, "SpatialGridDataFrame")
x[["idist"]] = 1 - x[["dist"]]
image(x["idist"])

str(x)

data(NLD_muni)



par(mar=c(1,1,1,1))
plot(NLD_lim)
image(x["idist"], add = TRUE)


grid.rect()

r <- raster(x, layer="idist")
plot(r)



bb <- bbox(NLD_lim)

grid.raster(r)

r2 <- as.raster(r)
grid.raster(r2, width=.5, height=.5)



data(NLD_muni)
NLD_lim <- NLD_muni[NLD_muni$province=="Limburg",]

data(meuse.grid)
str(meuse.grid)

r <- raster(meuse.grid)

tm_shape(NLD_muni) +
	tm_fill() +
	tm_borders() +
tm_shape(meuse.grid) +
	tm_raster("dist")


