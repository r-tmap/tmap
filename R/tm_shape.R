#' Shape (spatial object) specification
#' 
#' Specify a shape, which is a spatial object from one of these spatial object class packages: \code{sf}, \code{stars}, \code{terra}.
#' 
#' @param shp Spatial object
#' @param name Name of the shape
#' @param is.main Is \code{shp} the main shape, which determines the crs and bounding box of the map?
#' @param crs CRS to which \code{shp} is reprojected (only used if \code{is.main = TRUE})
#' @param bbox Bounding box of he map (only used if \code{is.main = TRUE})
#' @param unit Unit of the coordinates
#' @param filter Filter features
#' @param ... to catch deprecated arguments from version < 4.0
#' @import tmaptools
#' @import sf
#' @import stars
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
#' @import stats
#' @importFrom rlang missing_arg expr
#' @importFrom grDevices col2rgb colorRampPalette colors dev.off dev.size png rgb
#' @import utils
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
		message("The argument 'projection' is deprecated as of tmap 4.0. Pleaes use 'crs' instead", call. = FALSE)
		crs = args$projection
	}
	
	if (missing(shp)) {
		do.call(tm_options, args[intersect(names(args), c("bbox", "crs", "set.bounds", "set.view", "set.zoom.limits"))])
	} else {
		tm_element_list(tm_element(shp = shp,
								   is.main = is.main,
								   crs = crs,
								   bbox = bbox,
								   unit = unit,
								   filter = filter,
								   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name), 
								   subclass = "tm_shape"))
	}
}
