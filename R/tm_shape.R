#' @import tmaptools
#' @import sf
#' @import stars
#' @import units
#' @import grid
#' @import RColorBrewer
#' @import viridisLite
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
					filter = NULL) {
	tm_element_list(tm_element(shp = shp,
							   is.main = is.main,
							   crs = crs,
							   bbox = bbox,
							   unit = unit,
							   filter = filter,
							   shp_name = ifelse(is.null(name) == TRUE, deparse(substitute(shp))[1], name), 
							   subclass = "tm_shape"))
}
