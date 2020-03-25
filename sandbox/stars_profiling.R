profile <- function(e) {
	Rprof(tmp <- tempfile())
	eval(e)
	Rprof()
	print(summaryRprof(tmp))
	unlink(tmp)
}



profile(print(qtm(land) + tm_facets(by = "cover_cls")))


library(spDataLarge)
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

prec1 = dplyr::slice(prec, time, 1)

profile(print(tm_shape(prec1) + tm_raster()))









tm_shape(prec) + tm_raster() + tm_facets(free.scales = FALSE) # even slower



profile(print(tm_shape(World) + tm_polygons("HPI")))
