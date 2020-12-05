library(stars)
library(sf)
library(data.table)
library(pryr)

source("sandbox/test_data.R")



############ create tml

tml = tm_shape(land) +
tm_shape(World, name = "The World", is.main = TRUE) +
tm_polygons("economy") +
tm_shape(metro) +
tm_shape(a)



############# process shapes

tmls = tm_element_list_sel(tml, "tm_shape")



get_main_crs = function(tmls) {
	is_main = vapply(tmls, function(tme) {
		identical(tme$is.main, TRUE)
	}, FUN.VALUE = logical(1))
	
	main_id = if (any(is_main)) which(is_main)[1L] else 1L
	
	tms_main = tmls[[main_id]]
	
	crs_main = tms_main$crs
	if (is.null(crs_main)) crs_main = sf::st_crs(tms_main$shp)
	crs_main
}

crs = get_main_crs(tmls)

tmls = lapply(tmls, function(tms, crs) {
	tms$crs = crs
	tms
}, crs = crs)


x = tmls[[1]]

shp = x[[1]]
is.main = x[[2]]
crs = x[[3]]
bbox = x[[4]]
unit = x[[5]]
shp_name = x[[6]]


x$a=1

do.call(.tmapShape, tmls[[1]])


as.data.table(shp)
shp = sf::st_as_sf(shp)

has_raster(shp)
r = attr(shp, "raster")

attr(x, "raster")

get_raster(x)




get_empty_raster = function(shp, shp_name) {
	
	
	if (!has_raster(shp)) {
		dimnms = dimnames(shp)
		
		dimvals = lapply(1:length(dimnms), function(i) st_get_dimension_values(shp, i))
		dimsfc = vapply(dimvals, inherits, what = "sfc", FUN.VALUE = logical(1))
		
		if (!any(dimsfc)) {
			stop("stars object ", shp_name, " is a stars object without raster and doens't have a geometry dimension")
		} else {
			dimid = which(dimsfc)
			geoms = dimvals[[dimid]]
			dimnms_new = dimnms
			dimnms_new[dimid] = "tmapID__"
			shpnames = names(shp)
			shp = st_set_dimensions(shp, dimnms[dimid], values = 1:length(geoms))
			shp = st_set_dimensions(shp, names = dimnms_new)
		}
		
		data = as.data.table(shp)
		shp = geoms
		
	} else {
		shp <- downsample_stars(shp, max.raster = 1e5)
		if (sf::st_crs(shp) != crs) {
			shp <- transwarp(shp, crs = crs, raster.warp = TRUE)
		}
		
		
		dims = st_dimensions(shp)
		rst = attr(dims, "raster")
		dimsxy = dims[names(dims) %in% rst$dimensions]

		shp2 = st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
		shp3 = st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		
		data = as.data.table(shp3, center = FALSE)
		data[, tmapID__ := (y-1) * nrow(shp) + x]
		data[, x]
		
		m = matrix(1L:(nrow(shp) * ncol(shp)), nrow = nrow(shp), ncol = ncol(shp))
		
		data = 
		shp = st_as_stars(list(m = m), dimensions = dimsxy)
	}
	
	
	
}