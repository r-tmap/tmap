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
#' @import leaflet
#' @import leafsync
#' @import leafem
#' @import leaflegend
#' @import stats
#' @importFrom methods as
#' @importFrom rlang missing_arg expr
#' @importFrom s2 s2_buffer_cells as_s2_geography s2_intersects
#' @importFrom grDevices col2rgb colorRampPalette colors dev.off dev.size png rgb
#' @import utils
#' @note as of tmap 4.0, simplify has been removed. Please use [tmaptools::simplify_shape()] instead
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

	args_called = names(rlang::call_match()[-1])
	args = lapply(as.list(rlang::call_match(defaults = TRUE)[-1]), eval, envir = parent.frame())

	if ("projection" %in% args_called) {
		v3_instead_message(arg_old = "projection", arg_new = "crs", fun = "tm_shape")
		crs = args$projection
	}
	if (any(c("crs", "projection") %in% args_called) && is.na(is.main)) {
		is.main = TRUE
	}



	bbox_list = c(list(x = bbox), args[intersect(args_called, c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative", "asp.limit"))])

	shp_called = deparse(substitute(shp))[1]



	# rivers -> World_rivers
	if (shp_called %in% c("rivers", "c(735, 320, 325, 392, 524, 450, 1459, 135, 465, 600, 330, 336, ") && # last one to catch qtm calls
		is.numeric(shp) && length(shp) == 141) {
		World_rivers = NULL
		data(World_rivers, envir = environment())
		shp = World_rivers
		shp_called = "World_rivers"
		v3_rivers()
	}

	if ("simplify" %in% args_called) {
		sim_args = args[intersect(args_called, c("simplify", "keep.units", "keep.subunits"))]
		v3_simplify(shp_called, sim_args)
	}


	shp_name = ifelse(is.null(name) == TRUE, shp_called, name)


	if (missing(shp)) {
		do.call(tm_options, args[intersect(args_called, c("bbox", "crs", "set_bounds", "set_view", "set_zoom_limits"))])
	} else {
		tm_element_list(tm_element(shp = shp,
								   is.main = is.main,
								   crs = crs,
								   bbox = bbox_list,
								   unit = unit,
								   filter = filter,
								   shp_name = shp_name,
								   subclass = "tm_shape"))
	}
}
