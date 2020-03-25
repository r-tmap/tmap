library(devtools)
dev_mode()

library(microbenchmark)
library(profvis)
library(ggplot2)

library(sf)
library(stars)
library(mapview)
load_all()

data(land)

profvis({
	print(qtm(land) + tm_facets(by = "cover_cls"))
})


library(spDataLarge)
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

prec1 = dplyr::slice(prec, time, 1)


profvis({
	print(tm_shape(prec1) + tm_raster())
})


prec1_10 = dplyr::slice(prec, time, 1:10)


profvis({
	print(tm_shape(prec1_10) + tm_raster())
})





tm_shape(prec) + tm_raster() + tm_facets(free.scales = FALSE) # even slower



profile(print(tm_shape(World) + tm_polygons("HPI")))



profile(print(qtm(World)))


f <- function() {
	ifelse(sel, dat2, NA)
}

g <- function() {
	dat2[!sel] <- NA
	dat2
}

autoplot(microbenchmark(
	f(),
	g(),
	times = 10
))


