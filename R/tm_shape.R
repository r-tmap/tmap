#' Shape (spatial object) specification
#' 
#' Specify a shape, which is a spatial object from one of these spatial object class packages: \code{sf}, \code{stars}, \code{terra}.
#' 
#' @param shp spatial object
#' @param name name of the shape
#' @param is.main is \code{shp} the main shape, which determines the crs and bounding box of the map?
#' @param crs crs to which \code{shp} is reprojected (only used if \code{is.main = TRUE})
#' @param bbox bounding box of he map (only used if \code{is.main = TRUE})
#' @param unit unit of the coordinates
#' @param filter filter features
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
					name = NULL,
					is.main = NA,
					crs = NULL,
					bbox = NULL,
					unit = NULL,
					filter = NULL,
					projection = NULL) {
	if (!is.null(projection)) {
		warning("The argument 'projection' is deprecated as of tmap 4.0. Pleaes use 'crs' instead", call. = FALSE)
		crs = projection
		projection = NULL
	}
	tm_element_list(tm_element(shp = shp,
							   is.main = is.main,
							   crs = crs,
							   bbox = bbox,
							   unit = unit,
							   filter = filter,
							   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name), 
							   subclass = "tm_shape"))
}
