#' Quick thematic map plot
#'
#' Draw a thematic map quickly. This function is a convenient wrapper of the main
#' plotting method of stacking [`tmap-element`]s. Without arguments or with a
#' search term, this functions draws an interactive map.
#'
#' The first argument is a shape object (normally specified by [tm_shape()]).
#' The next arguments, from `fill` to `raster`, are the aesthetics from the main
#' layers. The remaining arguments are related to the map layout. Any argument
#' from any main layer function, such as [tm_polygons()], can be specified (see `...`).
#' It is also possible to stack [`tmap-element`]s on a `qtm` plot. See examples.
#'
#' By default, a scale bar is shown. This option can be set with [tmap_options()]
#' (argument `qtm.scalebar`). A minimap is shown by default when `qtm` is called
#' without arguments of with a search term. This option can be set with [tmap_options()]
#' (argument `qtm.minimap`).
#'
#' @param shp One of:
#'  * shape object, which is an object from a class defined by the [`sf`][`sf::sf`]
#'    or [`stars`][stars::st_as_stars()] package. Objects from the packages `sp`
#'    and `raster` are also supported, but discouraged.
#'  * Not specified, i.e. `qtm()` is executed. In this case a plain interactive
#'    map is shown.
#'  * An OpenStreetMap search string, e.g. `qtm("Amsterdam")`. In this case a
#'    plain interactive map is shown positioned according to the results of the
#'    search query (from OpenStreetMap nominatim)
#' @param fill,col,size,shape,lwd,lty,fill_alpha,col_alpha Visual variables.
#' @param text,text_col,text_size Visual variables.
#' @param by data variable name by which the data is split, or a vector of two
#'   variable names to split the data by two variables (where the first is used
#'   for the rows and the second for the columns). See also [tm_facets()].
#' @param scale numeric value that serves as the global scale parameter. All font
#'   sizes, symbol sizes, border widths, and line widths are controlled by this value.
#'   The parameters `symbols.size`, `text.size`, and `lines.lwd` can be scaled
#'   separately with respectively `symbols.scale`, `text.scale`, and
#'   `lines.scale`. See also `...`.
#' @param title main title. For legend titles, use `X.legend`, where X is the
#'   layer name (see `...`).
#' @param crs Either a [`crs`][sf::st_crs()] object or a character value
#'   (`PROJ.4` character string). By default, the projection is used that is
#'   defined in the `shp` object itself.
#' @param bbox bounding box. Argument passed on to [tm_shape()]
#' @param basemaps name(s) of the provider or an URL of a tiled basemap.
#'   It is a shortcut to [tm_basemap()]. Set to `NULL` to disable basemaps.
#'   By default, it is set to the tmap option `basemaps`.
#' @param overlays name(s) of the provider or an URL of a tiled overlay map.
#'   It is a shortcut to [tm_tiles()].
#' @param zindex zindex
#' @param group group
#' @param group.control group.control
#' @param style Layout options (see [tm_layout()]) that define the style.
#'   See [tmap_style()] for details.
#' @param format Deprecated, see [tm_format()] for alternatives
#' @param ... arguments associated with the visual variables are passed on
#'   to the layer functions [tm_polygons()], [tm_lines()], [tm_symbols()],
#'   and [tm_text()].
#'   For instance, `fill.scale` is the scale specifications of the fill color
#'   of polygons (see [tm_polygons()]).
#' @return A [`tmap-element`]
#' @example ./examples/qtm.R
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R},
#'   Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
#' @export
qtm = function(shp = NULL,
				fill = tmap::tm_const(),
				col = tmap::tm_const(),
				size = tmap::tm_const(),
				shape = tmap::tm_const(),
				lwd = tmap::tm_const(),
				lty = tmap::tm_const(),
				fill_alpha = tmap::tm_const(),
				col_alpha = tmap::tm_const(),
				text = tmap::tm_const(),
				text_col = tmap::tm_const(),
				text_size = tmap::tm_const(),
				by = NULL,
				scale = NULL,
				title = NULL,
				crs = NULL,
				bbox = NULL,
				basemaps = NA,
				overlays = NA,
				zindex = NA,
				group = NA,
				group.control = "check",
				style = NULL,
				format = NULL,
				...) {

	args = lapply(as.list(rlang::call_match(defaults = TRUE, dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	shp_name = deparse(substitute(shp))[1]
	args_called = names(rlang::call_match(dots_expand = TRUE)[-1])

	if (any(v3_only("qtm") %in% args_called)) {
		v3_start_message()
		args_new = list(args = args, called = args_called) |>
			v3_instead("symbols.size", "size", "qtm", extra_called = "shape") |>
			v3_instead("symbols.col", "size", "qtm", extra_called = "shape") |>
			v3_instead("dots.col", "fill", "qtm") |>
			v3_instead("lines.lwd", "lwd", "qtm") |>
			v3_instead("lines.col", "col", "qtm") |>
			v3_instead("raster", "col", "qtm") |>
			v3_instead_value("borders", "col", "qtm", value_old = NULL, value_new = NA) |>
			v3_instead("text.size", "text_size", "qtm") |>
			v3_instead("text.col", "text_col", "qtm") |>
			v3_instead("projection", "crs", "qtm")
		args = args_new$args
		args_called = args_new$called
	}

	o = tmap_options_mode(mode.specific = FALSE)
	show.warnings = o$show.warnings

	if (missing(shp) || is.character(shp)) {
		viewargs = args[intersect(names(args), names(formals(tm_view)))]
		if (!missing(shp)) viewargs$bbox = shp

		g = c(tm_basemap(basemaps), tm_tiles(overlays), do.call("tm_view", viewargs))
		if (o$qtm.minimap) g = c(g, tm_minimap())
		if (o$qtm.scalebar) g = c(g, tm_scalebar())
		if (o$qtm.mouse_coordinates) g = c(g, tm_mouse_coordinates())

		attr(g, "qtm_shortcut") = TRUE
		class(g) = "tmap"
		return(g)
	}


	if (!missing(shp)) {
		nms_shp = intersect(args_called, names(formals(tm_shape)))
		g = do.call(tm_shape, args[nms_shp])

		is_rst = inherits(shp, c("stars", "SpatRaster"))

		prefix = list(tm_polygons = "fill",
					  tm_lines = "lines",
					  tm_symbols = c("symbols", "dots"),
					  tm_raster = "raster")

		if (is_rst) {
			nms_rst = intersect(names(args), funs_v4$tm_raster)
			args_rst = args[nms_rst]

			if (!any(c("col", "raster") %in% args_called)) {
				args_rst$col = tm_vars()
			}

			nms_rst_v3 = names(args)[substr(names(args), 1, 7) == "raster."]
			args_rst_v3 = args[nms_rst_v3]
			names(args_rst_v3) = substr(names(args_rst_v3), 8, nchar(names(args_rst_v3)))

			g = g + do.call(tm_raster, c(args_rst, args_rst_v3))
		} else {

			for (f in c("tm_polygons", "tm_lines", "tm_symbols")) {
				nms_f = intersect(names(args), funs_v4[[f]])
				args_f = args[nms_f]

				nms_other = intersect(setdiff(names(args), c(nms_f, nms_shp, "basemaps", "overlays", "style", "format")), args_called)
				v3_nms = intersect(nms_other, unlist(lapply(prefix[[f]], function(p) paste(p, setdiff(v3_only(f), "scale"), sep = "."))))
				args_v3 = args[v3_nms]
				names(args_v3) = sub("^[^.]+[.]", "", names(args_v3))
				if (f == "tm_symbols") {
					if (!"shape" %in% args_called) args_f$shape = NULL
					if (!"col" %in% args_called) args_f$col = NULL
				}
				if (f == "tm_lines") {
					if (!"col" %in% args_called) args_f$col = NULL
				}

				args_f$called_from = "qtm"
				options = opt_tm_sf()[[c(tm_polygons = "polygons", tm_lines = "lines", tm_symbols = "points")[f]]]
				g = g + do.call(f, c(args_f, args_v3, list(options = options)))
			}
			if ("text" %in% args_called) {
				args_txt = args[substr(names(args), 1, 5) == "text_" | names(args) == "text"]
				names(args_txt) = sub("text_", "", names(args_txt))

				v3_nms = substr(names(args), 1, 5) == "text."
				args_v3 = args[v3_nms]
				names(args_v3) = sub("^[^.]+[.]", "", names(args_v3))

				args_f$called_from = "qtm"
				g = g + do.call(tm_text, c(args_txt, args_v3))
			}
		}

		nms_fct = intersect(names(args), names(formals(tm_facets)))
		if (length(nms_fct)) {
			g = g + do.call(tm_facets, args[nms_fct])
		}
	} else {
		g = NULL
	}


	if ("basemaps" %in% args_called || o$basemap.show) {
		g = g + do.call(tm_basemap, list(server = args$basemaps))
	}
	if ("tiles" %in% args_called || o$tiles.show) {
		g = g + do.call(tm_tiles, list(server = args$overlays))
	}
	if ("style" %in% args_called) {
		g = g + tm_style(args$style)
	}
	if ("title" %in% args_called) {
		g = g + tm_title(args$title)
	}

	if (o$qtm.scalebar) g = g + tm_scalebar()
	if (o$qtm.minimap) g = g + tm_minimap()
	if (o$qtm.mouse_coordinates) g = g + tm_mouse_coordinates()

	if (is.null(g)) {
		message_qtm_empty()
		invisible(NULL)
	} else {
		assign("last_map_new", rlang::call_match(), envir = .TMAP)
		attr(g, "qtm_shortcut") = FALSE
		g
	}
}
