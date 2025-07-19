#' Shape (spatial object) specification
#'
#' Specify a shape, which is a spatial object from one of these spatial object
#' class packages: [`sf`][`sf::sf`], [`stars`][stars::st_as_stars()], or `terra`.
#'
#' The map projection (`crs`) determines in which coordinate system the spatial object is processed and plotted.
#' See \href{https://r-tmap.github.io/tmap/articles/foundations_crs}{vignette about CRS}. The `crs` can be specified in two places: 1) `tm_shape()` and `tm_crs()`.
#' In both cases, the map is plotted into the specified `crs`. The difference is that in the first option, the `crs` is also taken into account in spatial transformation functions, such as the calculation of centroids and cartograms. In the second option, the `crs` is only used in the plotting phase.
#'
#' The automatic crs recommendation (which is still work-in-progress) is the following:
#'
#' | **Property**        | **Recommendation** |
#' | ------          | ----------- |
#' | `global` (for world maps)		     | A pseudocylindrical projection tmap option `crs_global`, by default `"eqearth` (Equal Earth). See \url{https://r-tmap.github.io/tmap/articles/41_advanced_crs.html} for more options|
#' | `area` (equal area)		     | Lambert Azimuthal Equal Area (`laea`) |
#' | `distance`	(equidistant) | Azimuthal Equidistant (`aeqd`) |
#' | `shape`	(conformal) | Stereographic (`stere`) |
#'
#' For further info about the available "generic" projects see:
#' for utm: \url{https://proj.org/en/9.4/operations/projections/utm.html}
#' for laea: \url{https://proj.org/en/9.4/operations/projections/laea.html}
#' for aeqd: \url{https://proj.org/en/9.4/operations/projections/aeqd.html}
#' for pconic: \url{https://proj.org/en/9.4/operations/projections/pconic.html}
#' for eqdc: \url{https://proj.org/en/9.4/operations/projections/eqdc.html}
#'
#' @param shp Spatial object
#' @param bbox Bounding box of the map (only used if `shp` is the main shape (see `is.main`). Three options: a [sf::st_bbox()] object, an Open Street Map query (passed on to [tmaptools::geocode_OSM()]), or `"FULL"`, which means the whole earth (this also guarantees that transformations to another CRS keep the whole earth, unlike [sf::st_bbox()]).
#' @param crs Map projection (CRS). Can be set to an `crs` object (see [sf::st_crs()]), a proj4string, an EPSG number, the value `"auto"` (automatic crs recommendation), or one the the following generic projections: `c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")`. See details.
#' @param is.main Is `shp` the main shape, which determines the crs and
#'   bounding box of the map? By default, `TRUE` if it is the first `tm_shape` call
#' @param name of the spatial object
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
#' @importFrom utils head tail download.file txtProgressBar str data capture.output setTxtProgressBar methods getS3method
#' @importFrom rlang missing_arg expr
#' @importFrom s2 s2_buffer_cells as_s2_geography s2_intersects
#' @importFrom grDevices col2rgb colorRampPalette colors dev.off dev.size png rgb
#' @note as of tmap 4.0, simplify has been removed. Please use [tmaptools::simplify_shape()] instead
#' @example ./examples/tm_shape.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/foundations_crs}{vignette about CRS}
#' @export
tm_shape = function(shp = NULL,
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
