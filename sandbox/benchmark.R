library(devtools)
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



