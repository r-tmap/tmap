library(raster)

x <- raster("../test/i01cz1.tif")
plot(x)

str(x)

data(NLD_muni)

qtm(x) + qtm(NLD_muni, fill = NA)



data(land)

landr <- as(land, "RasterLayer")
str(landr)
