library(devtools)
dev_mode()

library(microbenchmark)
library(ggplot2)

library(sf)
library(stars)
library(mapview)
load_all()


data(World)


tmap_mode("plot")
autoplot(microbenchmark(times = 5,
	plot(World[, "footprint"]),
	print(tm_shape(World) + tm_polygons("footprint"))
))

autoplot(microbenchmark(times = 5,
						print(qtm(World, fill = "footprint")),
						print(tm_shape(World) + tm_polygons("footprint"))
))




tmap_mode("view")
autoplot(microbenchmark(times = 5,
						print(qtm(World)),
						print(mapview(World))
))


autoplot(microbenchmark(times = 5,
						print(qtm(World, fill = "HPI")),
						print(mapview(World, zcol = "HPI"))
))


autoplot(microbenchmark(times = 1000,
						"A" %in% LETTERS,
						any("A" == LETTERS)))


library(spDataLarge)
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

prec1 = dplyr::slice(prec, time, 1)

autoplot(microbenchmark(times = 5,
						print(tm_shape(prec1) + tm_raster()),
						plot(prec1)))




