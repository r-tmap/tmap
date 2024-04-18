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
#' @param by data variable name by which the data is split, or a vector of two
#'   variable names to split the data by two variables (where the first is used
#'   for the rows and the second for the columns). See also [tm_facets()].
#' @param scale numeric value that serves as the global scale parameter. All font
#'   sizes, symbol sizes, border widths, and line widths are controlled by this value.
#'   The parameters `symbols.size`, `text.size`, and `lines.lwd` can be scaled
#'   separately with respectively `symbols.scale`, `text.scale`, and 
#'   `lines.scale`. See also `...`.
#' @param title main title. For legend titles, use `X.style`, where X is the
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
#' @param format Layout options (see [tm_layout()]) that define the format.
#'   See [tmap_format()] for details.
#' @param ... arguments passed on to the `tm_*` functions. The prefix of these
#'   arguments should be with the layer function name without `"tm_"` and a period.
#'   For instance, the palette for polygon fill color is called `fill.palette`.
#'   The following prefixes are supported: `shape.`, `fill.`, `borders.`, `polygons.`,
#'    `symbols.`, `dots.`, `lines.`, `raster.`, `text.`, `layout.`, `grid.`,
#'     `facets.`, and `view.`. Arguments that have a unique name, i.e. that does
#'     not exist in any other layer function, e.g. `convert2density`, can also be
#'     called without prefix.
#' @return A [`tmap-element`]
#' @example ./examples/qtm.R
#' @seealso `vignette("tmap_sneak_peek")`
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R},
#'   Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
#' @export
qtm <- function(shp, 
				fill = tm_const(),
				col = tm_const(),
				size = tm_const(),
				shape = tm_const(),
				lwd = tm_const(),
				lty = tm_const(),
				fill_alpha = tm_const(),
				col_alpha = tm_const(),
				text = tm_const(),
				text_col = tm_const(),
				text_size = tm_const(),
				by = NULL,
				scale = NULL,
				title = NULL,
				crs = NULL,
				bbox = NULL,
				basemaps = NULL,
				overlays = NULL,
				zindex = NA,
				group = NA,
				group.control = "check",
				style=NULL,
				format=NULL,
				...) {
	args <- c(as.list(environment()), list(...))
	shp_name <- deparse(substitute(shp))[1]
	called <- names(match.call(expand.dots = TRUE)[-1])
	
	vv_col = "col"
	vv_text = c("text", "text_col", "text_size")
	vv_oher = c("fill", "size", "shape", "lwd", "lty", "fill_alpha", "col_alpha")
	
	vv_attr = c(".scale", ".legend", ".free", ".chart")


	v3 = c("symbols.size", "symbols.col", "symbols.shape", "dots.col", 
	 "text.size", "text.col", "lines.lwd", "lines.col", "raster", 
	  "borders", "projection")
	
	if (any(v3 %in% names(args))) {
		mes = "tmap v3 code detected"
		
		if ("symbols.size" %in% names(args)) {
			args$size = args$symbols.size
			args$symbols.size = NULL
			called = unique(c(called, "shape"))
			mes = paste0(mes, "; use 'size' instead of 'symbols.size'")
		}
		if ("symbols.col" %in% names(args)) {
			args$fill = args$symbols.col
			args$symbols.col = NULL
			called = unique(c(called, "shape"))
			mes = paste0(mes, "; use 'fill' instead of 'symbols.col'")
		} else if ("dots.col" %in% names(args)) {
			args$fill = args$dots.col
			args$dots.col = NULL
			mes = paste0(mes, "; use 'fill' instead of 'dots.col'")
		}
		
		if ("lines.lwd" %in% names(args)) {
			args$lwd = args$lines.lwd
			args$lines.lwd = NULL
			mes = paste0(mes, "; use 'lwd' instead of 'lines.lwd'")
		}
		if ("lines.col" %in% names(args)) {
			args$col = args$lines.col
			args$lines.col = NULL
			mes = paste0(mes, "; use 'col' instead of 'lines.col'")
		}
		if ("raster" %in% names(args)) {
			args$col = args$raster
			args$raster = NULL
			mes = paste0(mes, "; use 'col' instead of 'raster'")
		}
		if ("borders" %in% names(args)) {
			if (is.null(args$borders)) {
				args$borders = NULL
				args$col = NA
				mes = paste0(mes, "; use 'col = NA' instead of 'borders = NULL'")	
			} else {
				args$col = args$borders
				args$borders = NULL
				mes = paste0(mes, "; use 'col' instead of 'borders'")	
			}
		}
		
		if ("text.size" %in% names(args)) {
			args$text_size = args$text.size
			args$text.size = NULL
			mes = paste0(mes, "; use 'text_size' instead of 'text.size'")
		}
		if ("text.col" %in% names(args)) {
			args$text_col = args$text.col
			args$text.col = NULL
			mes = paste0(mes, "; use 'text_col' instead of 'text.col'")
		}
		
		message(mes)
		
	}
	
	tmapOptions <- tmap_options_mode()
	show.warnings = tmapOptions$show.warnings
	
	if (missing(shp) || is.character(shp)) {
		
		viewargs <- args[intersect(names(args), names(formals(tm_view)))]
		
		if (!missing(shp)) viewargs$bbox <- shp
		
		g <- c(tm_basemap(basemaps), tm_tiles(overlays), do.call("tm_view", viewargs))
		attr(g, "qtm_shortcut") <- TRUE
		class(g) <- "tmap"
		return(g)
	}
	
	
	nms_shp = intersect(names(args), names(formals(tm_shape)))
	g = do.call(tm_shape, args[nms_shp])
	
	is_rst = inherits(shp, c("stars", "SpatRaster"))
	
	if (is_rst) {
		nms_rst = intersect(names(args), names(formals(tm_raster)))
		args_rst = args[nms_rst]
		
		if (!any(c("col", "raster") %in% called)) {
			args_rst$col = tm_shape_vars()
		}

		nms_rst_v3 = names(args)[substr(names(args), 1, 7) == "raster."]
		args_rst_v3 = args[nms_rst_v3]	
		names(args_rst_v3) = substr(names(args_rst_v3), 8, nchar(names(args_rst_v3)))
		
		g = g + do.call(tm_raster, c(args_rst, args_rst_v3))
	} else {
		nms_all = intersect(names(args), names(formals(tm_sf)))
		args_all =  args[nms_all]
		
		for (f in c("tm_polygons", "tm_lines", "tm_symbols")) {
			nms_f = intersect(names(args), names(formals(f)))
			args_f = args[nms_f]
			nms_other = intersect(setdiff(names(args), c(nms_f, nms_shp, "basemaps", "overlays", "style", "format")), called)
			args_other = args[nms_other]
			names(args_other) = sub("^[^.]+[.]", "", names(args_other))
			if (f == "tm_symbols") {
				if (!"shape" %in% called) args_f$shape = NULL
				if (!"col" %in% called) args_f$col = NULL
			}
			if (f == "tm_polygons") {
				if (length(args_other)) {
					# v3 confusion: tm_polygons used col while qtm used fill, therefore, tm_polygons will be called will col
					args_f$col = args_f$fill
				}
			}
			
			options = opt_tm_sf()[[c(tm_polygons = "polygons", tm_lines = "lines", tm_symbols = "points")[f]]]
			g = g + do.call(f, c(args_f, args_other, list(options = options)))
		}
		if ("text" %in% called) {
			args[substr(names(args), 1, 3) %in% c("col", "siz")] = NULL
			text_ = substr(names(args), 1, 5) == "text_"
			names(args)[text_] = substr(names(args)[text_], 6, nchar(names(args)[text_]))
			
			nms_f = intersect(names(args), names(formals(tm_text)))
			args_f = args[nms_f]
			nms_other = intersect(setdiff(names(args), c(nms_f, nms_shp, "basemaps", "overlays", "style", "format")), called)
			args_other = args[nms_other]
			names(args_other) = sub("^[^.]+[.]", "", names(args_other))
			g = g + do.call(tm_text, c(args_f, args_other))
		}
		
	}
	
	
	assign("last_map_new", match.call(), envir = .TMAP)
	attr(g, "qtm_shortcut") <- FALSE
	g
}
