data(World)
data(land)
data(metro)
data(rivers)


suppressPackageStartupMessages(library(dplyr))
library(stars)

prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
	st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
nc_outline = st_union(st_geometry(nc))
a = aggregate(prec, by = nc, FUN = max)
a


############ create tml

tml = tm_shape(land) +
tm_shape(World, name = "The World", is.main = TRUE) +
tm_polygons("economy") +
tm_shape(metro)



############# process shapes

tmls = tm_element_list_sel(tml, "tm_shape")


x = tmls[[1]]

shp = x[[1]]
is.main = x[[2]]
crs = x[[3]]
bbox = x[[4]]
unit = x[[5]]
shp_name = x[[6]]


do.call(.tmapShape, tmls[[1]])

library(stars)
library(sf)
library(data.table)

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
		dims = st_dimensions(shp)
		rst = attr(dims, "raster")
		dimsxy = dims[names(dims) %in% rst$dimensions]
		str(shp[[1]])
		
		
		(r = st_as_stars(list(m = m), dimensions = dimsxy))
	}
	
	
	
}