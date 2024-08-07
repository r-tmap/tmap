#' Shape (spatial object) specification
#' 
#' Specify a shape, which is a spatial object from one of these spatial object
#' class packages: [`sf`][`sf::sf`], [`stars`][stars::st_as_stars()], or `terra`.
#' 
#' @param shp Spatial object
#' @param name Name of the shape
#' @param is.main Is `shp` the main shape, which determines the crs and
#'   bounding box of the map? By default, `TRUE` if it is the first `tm_shape` call
#' @param crs CRS to which `shp` is reprojected (only used if `is.main = TRUE`)
#' @param bbox Bounding box of the map (only used if `shp` is the main shape (see `is.main`)
#' @param unit Unit of the coordinates
#' @param filter Filter features
#' @param ... passed on to \code{\link[tmaptools:bb]{bb}} (e.g. \code{ext} can be used to enlarge or shrink a bounding box)
#' @import tmaptools
#' @importFrom sf st_geometry st_sf st_as_sf st_transform st_crs
#' @importFrom stars st_dimensions st_get_dimension_values st_as_stars st_set_dimensions st_warp
#' @import units
#' @import grid
#' @import cols4all
#' @import classInt
#' @import htmltools
#' @import htmlwidgets
#' @import widgetframe
#' @import leaflet
#' @import leafsync
#' @import leafem
#' @import leaflegend
#' @import stats
#' @importFrom methods as
#' @importFrom rlang missing_arg expr
#' @importFrom grDevices col2rgb colorRampPalette colors dev.off dev.size png rgb
#' @import utils
#' @example ./examples/tm_shape.R  
#' @export
tm_shape = function(shp, 
					bbox = NULL,
					crs = NULL,
					is.main = NA,
					name = NULL,
					unit = NULL,
					filter = NULL,
					...) {
	args = as.list(match.call(expand.dots = TRUE)[-1])

	if ("projection" %in% names(args)) {
		v3_instead_message(arg_old = "projection", arg_new = "crs", fun = "tm_shape")
		crs = args$projection
	}
	
	bbox_list = c(list(x = bbox), args[intersect(names(args), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative", "asp.limit"))])
	
	if (missing(shp)) {
		do.call(tm_options, args[intersect(names(args), c("bbox", "crs", "set.bounds", "set.view", "set.zoom.limits"))])
	} else {
		tm_element_list(tm_element(shp = shp,
								   is.main = is.main,
								   crs = crs,
								   bbox = bbox_list,
								   unit = unit,
								   filter = filter,
								   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name), 
								   subclass = "tm_shape"))
	}
}
