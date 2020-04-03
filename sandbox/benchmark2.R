library(devtools)
library(microbenchmark)
library(ggplot2)
library(sf)
library(stars)
library(mapview)
library(raster)

options(viewer=NULL)

if (FALSE) {
	dev_mode()
	install_github("mtennekes/tmap") # most recent version (2020-04-02)
	# dev_mode(path = "~/R-dev_old")
	# install_github("mtennekes/tmap@895c4d4295") # version 2020-02-24 (3.0 before code improvements)
}

data(World, package = "tmap")
data(land, package = "tmap")

land2 <- st_as_stars(land)


##############################
mb <- list()

######## references

# sf
mb$sf <- microbenchmark(times = 5,
						plot(World[, "life_exp"]))
dev.off(dev.list()["RStudioGD"])

# stars
mb$stars <- microbenchmark(times = 5,
						   plot(land2))
dev.off(dev.list()["RStudioGD"])

# raster
mb$raster <- microbenchmark(times = 5,
							plot(land))
dev.off(dev.list()["RStudioGD"])

# mapview
mb$mapview <- microbenchmark(times = 3,
							 print(mapview(World, z = "life_exp")),
							 print(mapview(land, homebutton = FALSE)))


# tmap2_plot
tmap_mode("plot")
mb$tmap2 <- microbenchmark(times = 3,
							 print(tm_shape(World) + tm_polygons("life_exp")),
							 print(tm_shape(land) + tm_raster()))
dev.off(dev.list()["RStudioGD"])

# tmap2_view
tmap_mode("view")
mb$tmap2 <- microbenchmark(times = 3,
						   print(tm_shape(World) + tm_polygons("life_exp")),
						   print(tm_shape(land) + tm_raster()))


detach("package:tmap", unload = TRUE)
dev_mode(TRUE)
library(tmap)

# tmap3_plot
tmap_mode("plot")
mb$tmap3 <- microbenchmark(times = 3,
						   print(tm_shape(World) + tm_polygons("life_exp")),
						   print(tm_shape(land) + tm_raster()))
dev.off(dev.list()["RStudioGD"])

# tmap3_view
tmap_mode("view")
mb$tmap3 <- microbenchmark(times = 3,
						   print(tm_shape(World) + tm_polygons("life_exp")),
						   print(tm_shape(land) + tm_raster()))


