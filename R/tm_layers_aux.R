#' Map layer: basemap / overlay tiles
#'
#' Map layer that draws tiles from a tile server. `tm_basemap()` draws the tile
#' layer as basemap, i.e. as bottom layer. In contrast, `tm_tiles()` draws the
#' tile layer as overlay layer, where the stacking order corresponds with the
#' order in which this layer is called, just like other map layers.
#'
#'  API keys. For Stadia and Thunderforest maps, an API key is required.
#'  This can be set via the argument `api`. Alternatively they can be stored in environment variables `"STADIA_MAPS"` and `THUNDERFOREST_MAPS` with `Sys.setenv`
#'
#' @param server Name of the provider or an URL. Or a vector of multiple values. The list of available providers
#'   can be obtained with `providers` (tip: in RStudio, type `leaflet::providers$` to see
#'   the options). See <https://leaflet-extras.github.io/leaflet-providers/preview/>
#'   for a preview of those. When a URL is provided, it should be in template format,
#'   e.g. \code{"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}.
#'   Use `NULL` in `tm_basemap()` to disable basemaps. It can be a named vector. In that case these names will be used a group names, as alternative to the argument `group`.
#' @param alpha Transparency level
#' @param zoom Zoom level (only used in plot mode)
#' @param api API key. Needed for `Stadia` and `Thunderforest` maps in plot mode. See details
#' @param max.native.zoom Maximum native zoom level (only used in view mode).
#'   The minimum and maximum zoom levels are determined in `tm_view()`.
#' @param sub subdomain of the tile server. Only used when `server` is a url template. The
#'   default is `"abc"` which works for most tile servers.
#' @param zindex zindex of the pane in view mode. By default, it is set to the
#'   layer number plus 400. By default, the tmap layers will therefore be placed
#'   in the custom panes `"tmap401"`, `"tmap402"`, etc., except for the base tile
#'   layers, which are placed in the standard `"tile"`. This parameter determines
#'   both the name of the pane and the z-index, which determines the pane order
#'   from bottom to top. For instance, if `zindex` is set to 500, the pane will
#'   be named `"tmap500"`.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer
#'   groups can be switched on and off. Options: `"radio"` for radio buttons
#'   (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @export
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_basemaps}{Basemap examples}
#' @example ./examples/tm_basemap.R
tm_basemap = function(server = NA, alpha = NULL, zoom = NULL, api = NULL, max.native.zoom = 17, sub = "abc", zindex = 0, group = NA, group.control = "radio") {
	if (is.null(server)) {
		disable = TRUE
	} else {
		if (is.na(server[1])) {
			server = NULL # complete_options is used in step1 to find the default
		}
		disable = FALSE
	}

	group = check_basemap_group(server, group, called_from = "tm_basemap")


	tm_element_list(tm_element(
		args = list(server = server, alpha = alpha, zoom = zoom, api = api, max.native.zoom = max.native.zoom, sub = sub, type = "basemap", disable = disable),
		mapping.fun = "tm_aux_basemap",
		zindex = zindex,
		group = group,
		group.control = group.control,
		subclass = c("tm_basemap", "tm_aux_layer")))
}

#' @export
#' @rdname tm_basemap
tm_tiles = function(server = NA, alpha = NULL, zoom = NULL, max.native.zoom = 17, sub = "abc", zindex = NA, group = NA, group.control = "check") {
	if (is.null(server)) {
		disable = TRUE
	} else {
		if (is.na(server[1])) {
			server = NULL # complete_options is used in step1 to find the default
		}
		disable = FALSE
	}

	group = check_basemap_group(server, group, called_from = "tm_tiles")


	tm_element_list(tm_element(
		args = list(server = server, alpha = alpha, zoom = zoom, max.native.zoom = max.native.zoom, sub = sub, type = "overlay", disable = disable),
		mapping.fun = "tm_aux_tiles",
		zindex = zindex,
		group = group,
		group.control = group.control,
		subclass = c("tm_tiles", "tm_aux_layer")))
}

check_basemap_group = function(server, group, called_from) {
	if (is.null(server)) return(group)
	if (!is.na(group[1])) {
		if (length(group) != length(server)) {
			cli::cli_warn("{.field {.fun {called_from}}} multiple {.arg group} names specified, but unequal to the length of {.arg server}")
		} else {
			group = NA
		}

		if (!is.null(names(server))) {
			cli::cli_warn("{.field {.fun {called_from}}} {.arg group} specified, but {.arg server} is already a named vector, so {.arg group} will be ignored")
			group = NA
		}
	}
	group
}



#' @export
#' @rdname tm_grid
tm_graticules = function(x = NA,
						 y = NA,
						 n.x = NA,
						 n.y = NA,
						 crs = 4326,
						 labels.format = list(suffix = intToUtf8(176)),
						 labels.cardinal = TRUE,
						 ...) {
	do.call(
		tm_grid,
		c(list(x = x, y = y, n.x = n.x, n.y = n.y, crs = crs, labels.format = labels.format, labels.cardinal = labels.cardinal), list(...))
		)
}

#' Coordinate grid / graticule lines
#'
#' Draws horizontal and vertical lines according to a coordinate reference system (CRS). `tm_grid()` uses the CRS of the (master) shape object, and `tm_graticules()` uses latitude and longitude coordinates (EPSG 4326). It creates a [`tmap-element`] that draws coordinate grid lines. It serves as a
#' layer that can be drawn anywhere between other layers.
#'
#' @param x X coordinates for vertical grid lines. If `NA`, it is specified
#'   with a pretty scale and `n.x`.
#' @param y Y coordinates for horizontal grid lines. If `NA`, it is specified
#'   with a pretty scale and `n.y`.
#' @param n.x Preferred number of grid lines for the x axis. For the labels,
#'   a [pretty()] sequence is used, so the number of actual labels may be different than `n.x`.
#' @param n.y Preferred number of grid lines for the y axis. For the labels, a
#'   [pretty()] sequence is used, so the number of actual labels may be different than `n.y`.
#' @param crs Projection character. If specified, the grid lines are projected
#'   accordingly. Many world maps are projected, but still have latitude longitude
#'   (EPSG 4326) grid lines.
#' @param col Color of the grid lines.
#' @param lwd Line width of the grid lines
#' @param alpha Alpha transparency of the grid lines. Number between 0 and 1.
#'   By default, the alpha transparency of `col` is taken.
#' @param labels.show Show tick labels. Either one value for both `x` and `y` axis,
#'   or a vector two: the first for `x` and latter for `y`.
#' @param labels.pos position of the labels. Vector of two: the horizontal
#'   ("left" or "right") and the vertical ("top" or "bottom") position.
#' @param labels.size Font size of the tick labels
#' @param labels.col Font color of the tick labels
#' @param labels.rot Rotation angles of the labels. Vector of two values: the
#'   first is the rotation angle (in degrees) of the tick labels on the x axis
#'   and the second is the rotation angle of the tick labels on the y axis.
#'   Only `0`, `90`, `180`, and `270` are valid values.
#' @param labels.format List of formatting options for the grid labels. Parameters are:
#' \describe{
#' \item{fun}{Function to specify the labels. It should take a numeric vector,
#'   and should return a character vector of the same size.
#'   By default it is not specified. If specified, the list items `scientific`,
#'   `format`, and `digits` (see below) are not used.}
#' \item{scientific}{Should the labels be formatted scientifically?
#'   If so, square brackets are used, and the `format` of the numbers is `"g"`.
#'   Otherwise, `format="f"`, and `text.separator`, `text.less.than`, and
#'   `text.or.more` are used. Also, the numbers are automatically rounded to
#'   millions or billions if applicable.}
#' \item{format}{By default, `"f"`, i.e. the standard notation `xxx.xxx`, is used.
#'   If `scientific=TRUE` then `"g"`, which means that numbers are formatted scientifically,
#'   i.e. `n.dddE+nn` if needed to save space.}
#' \item{digits}{Number of digits after the decimal point if `format="f"`, and
#'   the number of significant digits otherwise.}
#' \item{...}{Other arguments passed on to [formatC()]}
#' }
#' @param labels.cardinal Add the four cardinal directions (N, E, S, W) to the labels,
#'   instead of using negative coordinates for west and south (so it assumes that
#'   the coordinates are positive in the north-east direction).
#' @param labels.margin.x Margin between tick labels of x axis and the frame.
#'   Note that when `labels.inside_frame = FALSE` and `ticks = TRUE`,
#'   the ticks will be adjusted accordingly.
#' @param labels.margin.y Margin between tick labels of y axis and the frame.
#'   Note that when `labels.inside_frame = FALSE` and `ticks = TRUE`,
#'   the ticks will be adjusted accordingly.
#' @param labels.space.x Space that is used for the labels and ticks for the x-axis
#'   when `labels.inside_frame = FALSE`. By default, it is determined automatically
#'   using the widths and heights of the tick labels. The unit of this parameter is text line height.
#' @param labels.space.y Space that is used for the labels and ticks for the y-axis
#'   when `labels.inside_frame = FALSE`. By default, it is determined automatically
#'   using the widths and heights of the tick labels. The unit of this parameter is text line height.
#' @param labels.inside_frame Show labels inside the frame? By default `FALSE`.
#' @param ticks If `labels.inside_frame = FALSE`, should ticks can be drawn between the labels and the frame?
#'   Either one value for both `x` and `y` axis, or a vector two: the first for `x` and latter for `y`.
#' @param lines If `labels.inside_frame = FALSE`, should grid lines can be drawn?
#' @param ndiscr Number of points to discretize a parallel or meridian
#'   (only applicable for curved grid lines)
#' @param zindex zindex of the pane in view mode. By default, it is set to the
#'   layer number plus 400. By default, the tmap layers will therefore be placed
#'   in the custom panes `"tmap401"`, `"tmap402"`, etc., except for the base tile
#'   layers, which are placed in the standard `"tile"`. This parameter determines
#'   both the name of the pane and the z-index, which determines the pane order
#'   from bottom to top. For instance, if `zindex` is set to 500,
#'   the pane will be named `"tmap500"`.
#' @param group Name of the group to which this layer belongs.
#'   This is only relevant in view mode, where layer groups can be
#'   switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups
#'   can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown),
#'   `"check"` for check boxes (so multiple groups can be shown), and `"none"`
#'   for no control (the group cannot be (de)selected).
#' @param ... Used to catch deprecated arguments from tmap v3.
#' @export
#' @example ./examples/tm_grid.R
tm_grid = function(x = NA,
				   y = NA,
				   n.x = NA,
				   n.y = NA,
				   crs = NA,
				   col = NA,
				   lwd = 1,
				   alpha = NA,
				   labels.show = TRUE,
				   labels.pos = c("left", "bottom"),
				   labels.size = .6,
				   labels.col = NA,
				   labels.rot = c(0, 0),
				   labels.format = list(big.mark = ","),
				   labels.cardinal = FALSE,
				   labels.margin.x = 0,
				   labels.margin.y = 0,
				   labels.space.x = NA,
				   labels.space.y = NA,
				   labels.inside_frame = FALSE,
				   ticks = labels.show & !labels.inside_frame,
				   lines = TRUE,
				   ndiscr = 100,
				   zindex = NA,
				   group = NA,
				   group.control = "none",
				   ...) {
	args = list(...)
	if ("projection" %in% names(args)) {
		cli::cli_warn("The 'projection' argument of 'tm_grid()' is deprecated as of tmap 4.0. Pleaes use 'crs' instead.", call. = FALSE)
		crs = args$projection
	}
	tm_element_list(tm_element(
		args = c(list(show = TRUE), as.list(environment())),
		mapping.fun = "tm_aux_grid",
		zindex = zindex,
		group = group,
		group.control = group.control,
		subclass = c("tm_grid", "tm_aux_layer")))
}
